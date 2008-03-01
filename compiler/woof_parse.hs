module WoofParse (readAST, WoofAST(..)) where
import WoofLexer 
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Monad

type WoofToken  = (SourcePos, Token)

type TokenParser a   = GenParser WoofToken () a

woofToken :: (Token -> Maybe a) -> TokenParser a
woofToken test
    = token showToken posToken testToken
      where
        showToken (pos,tok)   = show tok
        posToken  (pos,tok)   = pos
        testToken (pos,tok)   = test tok

readAST :: String -> WoofAST
readAST input = case parse parseModule "woof" (alexScanTokens input) of
                  Left err -> ASTNull ("No match: " ++ show err)
                  Right val -> val

data WoofAST = ASTNumber Integer
             | ASTString String
             | ASTInteger Integer
             | AST Bool
             | ASTSym String
             | ASTList [WoofAST]
             | ASTHash [WoofAST]
             | ASTBinaryFuncCall (WoofAST, WoofAST,WoofAST)
             | ASTFuncCall [(WoofAST, WoofAST)]
             | ASTPrimFuncCall (WoofAST, [WoofAST])
             | ASTBlock (WoofAST, WoofAST)
             | ASTEmptyBlock (WoofAST, WoofAST)
             | ASTFuncKey String
             | ASTPrimFuncKey String
             | ASTKeyVal (WoofAST, WoofAST)
             | ASTIdent String
             | ASTAssignment (WoofAST, WoofAST)
             | ASTConstant String
             | ASTGlobal String
             | ASTBool Bool
             | ASTNull String
             | ASTStatement WoofAST
             | ASTStatementList [WoofAST]
             | ASTFormalParams [WoofAST]
               deriving (Eq, Show)

parseModule :: TokenParser WoofAST
parseModule = do list <- parseStatementList
                 eof
                 return list

parseStatementList :: TokenParser WoofAST
parseStatementList = do liftM ASTStatementList $ sepBy parseStatement $ parseSym "."

parseStatement :: TokenParser WoofAST
parseStatement = do liftM ASTStatement parseExpr

parseIdent :: TokenParser WoofAST
parseIdent  = do liftM ASTIdent  $ woofToken (\tok -> case tok of
                                      TokIdent name -> Just name
                                      other -> Nothing)

parseConstant :: TokenParser WoofAST
parseConstant  = do liftM ASTConstant  $ woofToken (\tok -> case tok of
                                      TokConstant name -> Just name
                                      other -> Nothing)

parseGlobal :: TokenParser WoofAST
parseGlobal  = do liftM ASTGlobal  $ woofToken (\tok -> case tok of
                                      TokGlobal name -> Just (drop 1 name)
                                      other -> Nothing)

parseLitList :: TokenParser WoofAST
parseLitList = do parseSym "#"
                  parseSym "("
                  l <- liftM ASTList $ sepBy parseExpr $ parseSym ","
                  parseSym ")"
                  return l

parseLitHash :: TokenParser WoofAST
parseLitHash = do parseSym "#"
                  parseSym "("
                  h <- liftM ASTHash $ sepBy parseKeyVal $ parseSym ","
                  parseSym ")"
                  return h

parseFormalParams :: TokenParser WoofAST
parseFormalParams = do liftM ASTFormalParams $ sepBy parseIdent $ parseSym ","

parseLitBlock :: TokenParser WoofAST
parseLitBlock = do parseSym "["
                   params <- option (ASTFormalParams []) 
                             (do parseSym "|"
                                 params <- parseFormalParams
                                 parseSym "|"
                                 return $ params )
                   body <- parseStatementList
                   parseSym "]"
                   return $ case body of
                              (ASTStatementList []) -> ASTBlock (params,
                                                                 ASTStatementList [ASTConstant "nil"])
                              otherwise -> ASTBlock (params,body)

parseKeyVal :: TokenParser WoofAST
parseKeyVal = do k <- parseExpr
                 parseSym "=>"
                 v <- parseExpr
                 return $ ASTKeyVal $ (k,v)

parseAssignment :: TokenParser WoofAST
parseAssignment = do i <- (parseIdent <|> parseGlobal)
                     parseSym ":="
                     v <- parseExpr
                     return $ ASTAssignment $ (i,v)


parseExpr :: TokenParser WoofAST
parseExpr =  (try parseBinaryFuncCall)
             <|> parseConstant
             <|> (try parseAssignment <|> parseGlobal <|> parseIdent)
             <|> parseString
             <|> parseInteger                 
             <|> (try parseLitList <|> parseLitHash)
             <|> parseLitBlock
             <|> parsePrimFuncCall
             <|> parseFuncCall
             <|> parseParenExpr

parseBinaryArg :: TokenParser WoofAST
parseBinaryArg =  parseConstant
                 <|> (try parseAssignment <|> parseGlobal <|> parseIdent)
                 <|> parseString
                 <|> parseInteger                 
                 <|> (try parseLitList <|> parseLitHash)
                 <|> parseLitBlock
                 <|> parsePrimFuncCall
                 <|> parseFuncCall
                 <|> parseParenExpr

            
parseParenExpr :: TokenParser WoofAST
parseParenExpr = do parseSym "("
                    e <- parseExpr
                    parseSym ")"
                    return e

parseSym :: String -> TokenParser WoofAST
parseSym name = do liftM ASTSym  $ 
                         woofToken (\tok -> case tok of 
                                              TokSym s | s == name -> Just name
                                              other -> Nothing)

parseString :: TokenParser WoofAST
parseString = do liftM ASTString  $ 
                      woofToken (\tok -> case tok of 
                                           TokString s -> Just s
                                           other -> Nothing)


parseInteger :: TokenParser WoofAST
parseInteger = do liftM (ASTInteger . read)  $ 
                      woofToken (\tok -> case tok of 
                                           TokInteger s -> Just s
                                           other -> Nothing)


parseFuncKey :: TokenParser WoofAST
parseFuncKey = do liftM ASTFuncKey  $ 
                      woofToken (\tok -> case tok of 
                                           TokFuncKey s -> Just s
                                           other -> Nothing)

parseBinaryFuncKey :: TokenParser WoofAST
parseBinaryFuncKey = do liftM ASTFuncKey  $ 
                              woofToken (\tok -> case tok of 
                                                   TokBinaryFuncKey s -> Just s
                                                   other -> Nothing)

parseBinaryFuncCall :: TokenParser WoofAST
parseBinaryFuncCall = do chainl1 parseBinaryArg op
    where op = do f <- parseBinaryFuncKey
                  return (\left right -> ASTBinaryFuncCall (left, f, right))


parseFuncCall :: TokenParser WoofAST
parseFuncCall = do liftM ASTFuncCall  $ many1 (do s <- parseFuncKey
                                                  p <- parseExpr
                                                  return (s,p))


-- Parse primitives of the form @(prim_name a,b,c....)

parsePrimFuncKey :: TokenParser WoofAST
parsePrimFuncKey = do liftM ASTPrimFuncKey  $ 
                            woofToken (\tok -> case tok of 
                                                 TokPrimFuncKey s -> Just s
                                                 other -> Nothing)

parsePrimFuncCall :: TokenParser WoofAST
parsePrimFuncCall = do liftM ASTPrimFuncCall  $ do s <- parsePrimFuncKey
                                                   l <- sepBy parseExpr $ parseSym ","
                                                   parseSym ")" 
                                                   return (s,l)
