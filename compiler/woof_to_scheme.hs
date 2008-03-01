module WoofToScheme (emitScheme) where
import Monad
import System.Environment
import WoofParse

-- Environment

emitScheme :: WoofAST -> IO ()
emitScheme (ASTStatementList (statement:rest)) = do emitScheme statement
                                                    emitScheme $ ASTStatementList rest
emitScheme (ASTStatementList []) = do return ()

emitScheme (ASTStatement a) = do emitScheme a
                                 putStr "\n"

emitScheme (ASTIdent s) = do putStr $ "var_" ++ s

emitScheme (ASTAssignment (i, e)) = do putStr $ "(define "
                                       emitScheme i
                                       emitScheme $ e
                                       putStr ")"

emitScheme (ASTConstant s) = do putStr $ "(constant-by-name \"" ++ s++ "\")"

emitScheme (ASTString s) = do putStr $ "(woof-string-instance \"" ++ (content s) ++ "\")"
    where content t = drop  1 (take ((length  t) - 1) t)


emitScheme (ASTBlock ((ASTFormalParams params), statements)) = do putStr $ "(woof-block-instance (lambda ("
                                                                  emitList params
                                                                  putStr ")"
                                                                  emitScheme statements
                                                                  putStr "))"
    where emitList (first:rest) = do emitScheme first
                                     putStr " "
                                     emitList rest
          emitList otherwise = do putStr ""

emitScheme (ASTList list) = do putStr $ "(woof-list-instance (list "
                               emitList list
                               putStr "))"
    where emitList (first:rest) = do emitScheme first
                                     putStr " "
                                     emitList rest
          emitList otherwise = do putStr ""

emitScheme (ASTInlineScheme s) = do putStr $ content s
    where content t = drop  2 (take ((length  t) - 1) t)

emitScheme (ASTInteger i) = do putStr $ "(woof-integer-instance " ++ (show i) ++ ")"

emitScheme func@(ASTFuncCall funcList) = do putStr $ "(woof-call-function \"" ++ (functionName func) ++ "\" (list "
                                            emitArgList funcList
                                            putStr "))"
    where emitArgList ((key,val):rest) = do emitScheme val 
                                            putStr " "
                                            emitArgList rest
          emitArgList otherwise = do putStr ""


emitScheme ast = do putStrLn $ show ast

functionName :: WoofAST -> String
functionName (ASTFuncCall keyValPairs) = foldl sumKey "" keyValPairs 
    where sumKey sumString (ASTFuncKey key,val) = sumString ++ key

