module Types (
  LispVal(..),
  unwordsList,
  Env,
  nullEnv,
  isBound,
  getVar,
  setVar,
  defineVar,
  bindVars,
  LispError(..),
  ThrowsError,
  IOThrowsError,
  -- throwError,
  -- catchError,
  trapError,
  extractValue,
  liftThrows,
  runIOThrows
) where

import Data.IORef
import Control.Monad
import Control.Monad.Except
import Text.ParserCombinators.Parsec (ParseError)

-- LispVal       needs: LispVal, Env
-- Env           needs: LispVal, IOThrowsError
-- LispError     needs: LispVal
-- IOThrowsError needs: LispError

data LispVal = Atom String
            | List [LispVal]
            | DottedList [LispVal] LispVal
            | Number Integer
            | String String
            | Bool Bool
            | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
            | Func {
              params :: [String],
              vararg :: (Maybe String),
              body :: [LispVal],
              closure :: Env
            }

instance Show LispVal where
  -- show :: LispVal -> String
  show (String contents) = "\"" ++ contents ++ "\""
  show (Atom name) = name
  show (Number contents) = show contents
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (List contents) = "(" ++ unwordsList contents ++ ")"
  show (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"
  show (PrimitiveFunc _) = "<primitive>"
  show (Func params varargs _ _) =
    "(lambda (" ++
    unwords (map show params) ++
    (case varargs of
      Nothing -> ""
      Just arg -> " . " ++ arg) ++
    ") ...)"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = do
  env <- readIORef envRef
  return $ maybe False (const True) $ lookup var env

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Getting an unbound variable" var)
        (liftIO . readIORef)
        (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
  env <- liftIO $ readIORef envRef
  maybe (throwError $ UnboundVar "Setting an unbound variable" var)
        (liftIO . (flip writeIORef value))
        (lookup var env)
  return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings  = do
  env <- readIORef envRef
  newEnv <- extendEnv bindings env
  newIORef newEnv
  where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do
          ref <- newIORef value
          return (var, ref)

type ThrowsError = Either LispError

data LispError = NumArgs Integer [LispVal]
              | TypeMismatch String LispVal
              | Parser ParseError
              | BadSpecialForm String LispVal
              | NotFunction String String
              | UnboundVar String String
              | Default String

instance Show LispError where
  -- show :: LispError -> String
  show (UnboundVar message varname)  = message ++ ": " ++ varname
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction message func)    = message ++ ": " ++ show func
  show (NumArgs expected found)      = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
  show (Parser parseErr)             = "Parse error at " ++ show parseErr

-- throwError :: LispError -> ThrowsError a
-- throwError err = Left err

-- catchError :: ThrowsError a -> (LispError -> ThrowsError a) -> ThrowsError a
-- catchError (Left e) handler = handler e
-- catchError (Right v) handler = return v

-- http://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Except.html
-- trapError :: ThrowsError String -> ThrowsError String
-- trapError :: IOThrowsError String -> IOThrowsError String
-- trapError :: Monad m => MonadError a m -> MonadError a m
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

type IOThrowsError = ExceptT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue
