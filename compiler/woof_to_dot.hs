module WoofToDot (emitDotFile) where
import Monad
import System.Environment
import WoofParse
import Data.List
import Control.Exception
import Control.Monad.State

type IOTCounter = StateT GlobalCounter IO 
data GlobalCounter = GlobalCounter Integer
                     deriving (Eq, Show)

emitDotFile :: WoofAST -> IO ()
emitDotFile ast = do putStr $ concat [
                                      "digraph woof_program_tree {\n",
                                      "size=\"30,30\";",
	                              "node [color=lightblue2, style=filled];\n"
                                     ]
                     putStr $ "program1 -> "
                     finalCounter <- runStateT (emitDot ast) (GlobalCounter 0)
                     putStr "}\n"
                     return ()

liftPutStr :: String -> IOTCounter ()
liftPutStr str = do liftIO $ putStr str 

emitHead :: String -> Integer -> IOTCounter ()
emitHead str i = do liftPutStr $ concat ["\"",str,(show i),"\""," -> "]

emitTail :: String -> Integer -> IOTCounter ()
emitTail str i = do liftPutStr $ concat ["\"",str,(show i),"\"",";\n"]

emitChild :: String -> Integer -> WoofAST -> IOTCounter ()
emitChild str i ast = do emitHead str i
                         emitDot ast

nextId :: IOTCounter Integer
nextId = do GlobalCounter i <- get
            put (GlobalCounter  $ i + 1)
            return $ i + 1


emitDot :: WoofAST -> IOTCounter ()
               
emitDot (ASTStatementList list) = do i <- nextId
                                     emitTail "StatementList" i
                                     mapM (\st -> do emitChild "StatementList" i st) list
                                     return ()
                                           
emitDot (ASTStatement a) = do i <- nextId
                              emitTail "Statement" i
                              emitChild "Statement" i a

emitDot (ASTIdent s) = do i <- nextId
                          emitTail ("Ident_" ++ s) i

emitDot (ASTConstant s) = do i <- nextId
                             emitTail ("Const_" ++ s) i

emitDot (ASTGlobal s) = do i <- nextId
                           emitTail ("Global_" ++ s) i

emitDot (ASTString s) = do i <- nextId
                           emitTail ("Str_" ++ (content s)) i
    where content t = drop  1 (take ((length  t) - 1) t)

emitDot (ASTInteger i) = do i <- nextId
                            emitTail ("Int_" ++ (show i)) i


emitDot (ASTAssignment (ident, expr)) = do i <- nextId
                                           emitTail "Assignment" i
                                           emitChild "Assignment" i ident
                                           emitChild "Assignment" i expr

emitDot func@(ASTFuncCall funcList) 
    = do i <- nextId
         emitTail ("Call_" ++ (functionName func)) i
         emitHead ("Call_" ++ (functionName func)) i
         emitTail "Args" i
         mapM (\arg -> do emitChild "Args" i arg) (map (\(key,val) -> val) funcList)
         return ()



emitDot (ASTBlock ((ASTFormalParams params), statementList@(ASTStatementList statements)))
    = do i <- nextId
         emitTail "Block" i
         emitChild "Block" i statementList
         emitHead "Block" i
         emitTail "Params" i
         mapM (\p -> do emitChild "Params" i p) params
         return ()


emitDot (ASTList list) = do i <- nextId
                            emitTail "List" i
                            mapM (\ea -> do emitChild "List" i ea) list
                            return ()

emitDot func@(ASTBinaryFuncCall (arg1, ASTFuncKey op, arg2))
    = do i <- nextId
         emitTail ("Call_" ++ op) i
         emitHead ("Call_" ++ op) i
         emitTail "Args" i
         emitChild "Args" i arg1
         emitTail "Args" i
         emitChild "Args" i arg2
         return ()


emitDot func@(ASTPrimFuncCall (ASTPrimFuncKey key, list)) =
    do i <- nextId
       emitTail ("Call_Prim_" ++ (drop 2 key)) i
       emitHead ("Call_Prim_" ++ (drop 2 key)) i
       emitTail "Args" i
       mapM (\arg -> do emitChild "Args" i arg) list
       return ()

emitDot ast = do i <- nextId
                 liftPutStr $ "UNKNOWN" ++ (show i) ++ ";\n" 


functionName :: WoofAST -> String
functionName (ASTFuncCall keyValPairs) = foldl sumKey "" keyValPairs 
    where sumKey sumString (ASTFuncKey key,val) = sumString ++ "_" ++ (filter (not . (== ':')) key)

