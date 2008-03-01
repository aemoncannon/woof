module WoofEval (eval) where




data WoofVal = WoofObjectRef Integer
               deriving (Eq)

showVal :: WoofVal -> String
showVal (WoofObjectRef num) = "Obj:" ++ (show num)

instance Show WoofVal where show = showVal

data WoofError = NumArgs Integer [WoofVal]
               | TypeMismatch String WoofVal
               | Parser ParseError
               | BadSpecialForm String WoofVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: WoofError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected 
                                  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show WoofError where show = showError
instance Error WoofError where
     noMsg = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either WoofError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val