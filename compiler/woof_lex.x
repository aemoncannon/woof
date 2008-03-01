{
module WoofLexer where
import Text.ParserCombinators.Parsec.Pos
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$nonalpha = [\(\)_':]
@escape      = \\ ([\\\'\"\?])
@stringChars     = $white | ~\' | @escape
$commentChars = ~\"

tokens :-

  $white+				;
  "--".*				;
  [\.\"\,\[\]\=\(\)\|\#]		{ \(AlexPn o l c) s -> ((newPos "File" l c),TokSym s)} 
  "=>"			  	       	{ \(AlexPn o l c) s -> ((newPos "File" l c),TokSym s)} 
  ":="			  	       	{ \(AlexPn o l c) s -> ((newPos "File" l c),TokSym s)}

  "=="			  	       	{ \(AlexPn o l c) s -> ((newPos "File" l c),TokBinaryFuncKey s)}
  "!="			  	       	{ \(AlexPn o l c) s -> ((newPos "File" l c),TokBinaryFuncKey s)}
  ">="			  	       	{ \(AlexPn o l c) s -> ((newPos "File" l c),TokBinaryFuncKey s)}
  [\+\-\*\/\%\<\>]			{ \(AlexPn o l c) s -> ((newPos "File" l c),TokBinaryFuncKey s)}


  [$digit]+				{ \(AlexPn o l c) s -> ((newPos "File" l c),TokInteger s)} 
  $alpha [$alpha $digit _]*[\?\!]?\:	{ \(AlexPn o l c) s -> ((newPos "File" l c),TokFuncKey s)} 
  \$ [$alpha $digit _]*			{ \(AlexPn o l c) s -> ((newPos "File" l c),TokGlobal s) }
  [A-Z] [$alpha $digit _]*		{ \(AlexPn o l c) s -> ((newPos "File" l c),TokConstant s) }
  "true"	       			{ \(AlexPn o l c) s -> ((newPos "File" l c),TokConstant s) }
  "false"	       			{ \(AlexPn o l c) s -> ((newPos "File" l c),TokConstant s) }
  "nil"	       			 	{ \(AlexPn o l c) s -> ((newPos "File" l c),TokConstant s) }

  $alpha [$alpha $digit _]*		{ \(AlexPn o l c) s -> ((newPos "File" l c),TokIdent s) }

  \' @stringChars* \'	        { \(AlexPn o l c) s -> ((newPos "File" l c),TokString s) } 

  \" [$white $commentChars]* \"        ;

  @\($alpha [$alpha $digit _]*		{ \(AlexPn o l c) s -> ((newPos "File" l c),TokPrimFuncKey s) }



{
-- Each action has type :: String -> Token

-- The token type:

data Token =
        TokInteger String |      	   			
	TokConstant String |
	TokGlobal String |
	TokIdent String |
	TokSym String |
	TokString String |
	TokBinaryFuncKey String |
	TokFuncKey String |
	TokPrimFuncKey String
	deriving (Eq,Show)

testLex = do
  s <- getContents
  print (alexScanTokens s)

}