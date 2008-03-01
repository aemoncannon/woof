module Main where
import System.Environment
import WoofParse
import WoofToDot
import IO


main :: IO ()
main = do s <- hGetContents stdin
          ast <- return $ readAST s
          case ast of
            ASTNull errMsg -> do putStr errMsg
            otherwise -> emitDotFile ast


