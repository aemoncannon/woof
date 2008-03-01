module WoofLexicalAnalysis (lexAnalyze, ScopeInfo(..), WoofScopedAST) where
import Monad
import System.Environment
import WoofParse


data ScopeInfo = LexicalInfo Integer
                 deriving (Eq, Show)

type WoofScopedAST  = (ScopeInfo, WoofAST)

lexAnalyze :: WoofAST -> WoofScopedAST

lexAnalyze ast = analyzeAST (LexicalInfo 1, ast)

analyzeAST :: WoofScopedAST -> WoofScopedAST

analyzeAST (scope, block@(ASTBlock ((ASTFormalParams params), statements))) = WoofScopedAST (LexicalInfo 2, block)

analyzeAST (scope, ast@(ASTStatementList (statement:rest))) = WoofScopedAst (scope, (analyzeAST statement) ++ (analyzeAST rest))

analyzeAST (scope, ast@(ASTStatementList [])) = WoofScopedAST (scope, ast)

analyzeAST (scope, ast@(ASTStatement a)) = analyzeAST (scope, a)

analyzeAST (scope, ast@(ASTFuncCall funcList)) = analyzeAST (scope, ast)
    where emitArgList ((key,val):rest) = analyzeAST (scope, val)
          emitArgList otherwise = do putStr ""

analyzeAST (scope, ast) = (scope, ast)
              


