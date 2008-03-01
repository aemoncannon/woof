module WoofEval (eval, evalProg) where
import Data.IORef
import Control.Monad.Error
import Monad
import System.Environment
import WoofParse

-- Environment

evalProg :: WoofAST -> IO String
evalProg ast = runIOThrows $ do env <- nullWoofEnv
                                val <- eval env ast
                                return $ show val

eval :: Env -> WoofAST -> IOThrowsError WoofVal
eval env (ASTString s) = do return $ String "sdsd"


nullWoofEnv :: IOThrowsError Env
nullWoofEnv = do env <- liftIO nullEnv
                 defineVar env "nil" (WoofObject (0, String "nil", [], nullEnv))
                 metaObject <- getVar env "MetaObject"
                 defineVar env "MetaObject" (WoofObject (1, nilObject, [nilObject], nullEnv))
                 nil <- getVar env "nil"
                 defineVar env "Object"  (WoofObject (2, metaObject, [nil], nullEnv))
                 object <- getVar env "Object"
                 defineVar env "MetaString"  (WoofObject (2, nilObject, [nil], nullEnv))
                 metaString <- getVar env "metaString"
                 defineVar env "String"  (WoofObject (2, metaString, [object], nullEnv))
                 return env

stringInstance :: Env -> String -> IOThrowsError WoofVal
stringInstance env s = do stringClass <- getVar env "String"
                          objectClass <- getVar env "Object"
                          return $ WoofObject (3, stringClass, [objectClass], newIORef)

nilObject :: WoofVal
nilObject = WoofObject (0, nilObject, [], nullEnv)

type Env = IORef [(String, IORef WoofVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ErrorT WoofError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError WoofVal
getVar envRef var  =  do env <- liftIO $ readIORef envRef
                         maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                               (liftIO . readIORef)
                               (lookup var env)

setVar :: Env -> String -> WoofVal -> IOThrowsError WoofVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var) 
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> WoofVal -> IOThrowsError WoofVal
defineVar envRef var value = do 
    alreadyDefined <- liftIO $ isBound envRef var 
    if alreadyDefined 
       then setVar envRef var value >> return value
       else liftIO $ do 
          valueRef <- newIORef value
          env <- readIORef envRef
          writeIORef envRef ((var, valueRef) : env)
          return value

bindVars :: Env -> [(String, WoofVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)

-- Woof runtime types

data WoofVal = WoofObject (Integer,WoofVal,[WoofVal],IO Env)
             | String String
             | List [WoofVal]

instance Eq WoofVal where 
    (WoofObject (i, v, l, e)) == (WoofObject (i1, v1, l1, e1)) = 
        (v == v1)
    
showVal :: WoofVal -> String
showVal (WoofObject (i, v, l, e)) = "Obj:" ++ (show i)
showVal (String s) = show s

instance Show WoofVal where show = showVal

unwordsList :: [WoofVal] -> String
unwordsList = unwords . map showVal

-- Woof runtime errors

data WoofError = NumArgs Integer [WoofVal]
               | TypeMismatch String WoofVal
               | UnboundVar String String
               | Default String

showError :: WoofError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (NumArgs expected found) = "Expected " ++ show expected 
                                  ++ " args; found values " ++ unwordsList found

instance Show WoofError where show = showError

instance Error WoofError where
     noMsg = Default "An error has occurred"
     strMsg = Default

type ThrowsError = Either WoofError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val