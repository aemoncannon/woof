-- TODO: Need good errors for lexer....

module WoofToBC (emitBC) where
import Monad
import System.Environment
import WoofParse
import Data.List
import Control.Exception
import Control.Monad.State

data BCInstr = Instr (String, [String])
data LexicalEnv = Env [[String]]
                  deriving (Eq, Show)

type CompIO = StateT CompilerState IO 
data CompilerState = CompilerState LexicalEnv
                     deriving (Eq, Show)

lookupLexical :: LexicalEnv -> String -> Maybe (Integer,Integer)
lookupLexical (Env env) str  = findInFrames env 0
    where findInFrames (frame:rest) i = case elemIndex str frame of
                                          Just j -> Just (i, fromIntegral j)
                                          Nothing -> findInFrames rest (i + 1)
          findInFrames [] i = Nothing

pushNewLexicalFrame :: LexicalEnv -> LexicalEnv
pushNewLexicalFrame (Env env) = Env $ [[]] ++ env

popLexicalFrame :: LexicalEnv -> LexicalEnv
popLexicalFrame (Env env) = Env $ drop 1 env

appendLexical :: LexicalEnv -> String -> LexicalEnv
appendLexical (Env env) str = 
    let frame = head env
        newFrame = frame ++ [str] in
    Env ([newFrame] ++ (tail env))
                                
emitInstr :: BCInstr -> IO ()
emitInstr (Instr (instr, args)) = do putStr "("
                                     putStr instr
                                     putStr " "
                                     mapM emitArg args
                                     putStr ")\n"
                                     return ()
    where emitArg arg = do putStr arg
                           putStr " "

emitBC :: WoofAST -> IO ()
emitBC ast = do (bc, state) <- runStateT (genBC ast) (CompilerState (Env [[]]))
                mapM emitInstr $ bc ++ [Instr ("HALT", [])]
                return ()

genBC :: WoofAST -> CompIO [BCInstr]

genBC (ASTStatementList statements@(statement:rest)) = 
    do statementBC <- genBC statement
       restBC <- liftM concat $ mapM withPop rest
       return $ statementBC ++ restBC
    where withPop s = do bc <- genBC s
                         return $ [Instr ("POP", [])] ++ bc

genBC (ASTStatement a) = do statementBC <- genBC a
                            return statementBC

genBC (ASTIdent s) = 
    do CompilerState env <- get 
--       liftIO $ putStr $ "\nLooking up " ++ s ++ " in " ++ (show env) ++ "\n"
       maybeAddr <- return $ lookupLexical env s
       return $ case maybeAddr of
                  Just (i,j) -> [Instr ("LVAR", [show i, show j])]
                  Nothing -> error $ "Error: undeclared variable " ++ s --Should throw an exception


genBC (ASTAssignment ((ASTIdent s, e))) = 
    do (CompilerState env) <- get 
       maybeAddr <- return $ lookupLexical env s
       assignBC <- case maybeAddr of
                     (Just (i,j)) -> do return [Instr ("LSET", [show i,show j])]
                     Nothing -> do put (CompilerState (appendLexical env s))
                                   CompilerState env <- get
                                   return $ case lookupLexical env s of
                                              Just (i,j) -> [Instr ("LSET", [show i, show j])]
                                              Nothing -> error $ "Error: could not append lexical " ++ s
       expBC <- genBC e 
       return $ expBC ++ assignBC

genBC (ASTAssignment ((ASTGlobal s, e))) =
    do expBC <- genBC e 
       assignBC <- return [Instr ("SETG", ["\"" ++ s ++ "\""])]
       return $ expBC ++ assignBC

genBC (ASTConstant s) = do return [Instr ("GETC", ["\"" ++ s ++ "\""])]

genBC (ASTGlobal s) = do return [Instr ("GETG", ["\"" ++ s ++ "\""])]

genBC (ASTString s) = do return [Instr ("STRL", ["\"" ++ content s ++ "\""])]
    where content t = drop  1 (take ((length  t) - 1) t)

genBC (ASTInteger i) = do return [Instr ("INTL", [show i])]

genBC (ASTBlock ((ASTFormalParams params), statementList@(ASTStatementList statements)))
    = do (CompilerState env) <- get
         put (CompilerState (pushNewLexicalFrame env))
         installParams $ reverse params
         codeBC <- genBC statementList
         (CompilerState env) <- get
         put (CompilerState (popLexicalFrame env))
         let blockBC = [Instr ("BLKL", [show $ (2 + length codeBC)])]
             returnBC = [Instr ("RETURN",[])] in do
           return $  blockBC ++ codeBC ++ returnBC
    where installParams ((ASTIdent str):rest) = do (CompilerState env) <- get
                                                   put (CompilerState (appendLexical env str))
                                                   installParams rest
          installParams [] = do return ()

genBC (ASTList list) = do memberBC <- liftM concat $ mapM genBC (reverse list)
                          return $ memberBC ++ [Instr ("LSTL", [show $ length list])]

genBC func@(ASTFuncCall funcList) 
    = do argBC <- liftM concat $ mapM genBC (map (\(key,val) -> val) funcList)
         let funcBC = [Instr ("FUNC",["\"" ++ functionName func ++ "\"", show $ length funcList])]
             callBC = [Instr ("CALLJ",[show $ length funcList])] in do
           return $ argBC ++ funcBC ++ callBC

genBC func@(ASTBinaryFuncCall (arg1, ASTFuncKey op, arg2))
    = do arg1BC <- genBC arg1
         arg2BC <- genBC arg2
         let argBC = arg1BC ++ arg2BC
             funcBC = [Instr ("FUNC",["\"" ++ op ++ "\"", "2"])]
             callBC = [Instr ("CALLJ",["2"])] in do
           return $ argBC ++ funcBC ++ callBC

genBC func@(ASTPrimFuncCall (ASTPrimFuncKey key, list)) =
    let primKey = drop 2 key in
    case primKey of 
      "call_block" -> do blockBC <- genBC $ head list
                         argsBC <- genBC $ head $ tail list
                         let callBC = [Instr ("LSTCALLJ", [])] in do
                           return $ argsBC ++ blockBC ++ callBC

      "with_cc" -> do blockBC <- genBC $ head list
                      let argsBC = [Instr ("SAVECC", [show $ (2 + (length blockBC))])]
                          callBC = [Instr ("CALLJ", [show $ length list] )] in do
                        return $ argsBC ++ blockBC ++ callBC

      "return_cc" -> do ccBC <- genBC $ head list
                        argBC <- genBC $ head $ tail list
                        let retBC = [Instr ("RETCC", [] )] in do
                          return $ ccBC ++ argBC ++ retBC

      otherwise -> do argBC <- liftM concat $ mapM genBC (reverse list)
                      return $ argBC ++ [Instr ("PRIM", [primKey] )]

genBC ast = return $ []

functionName :: WoofAST -> String
functionName (ASTFuncCall keyValPairs) = foldl sumKey "" keyValPairs 
    where sumKey sumString (ASTFuncKey key,val) = sumString ++ key

