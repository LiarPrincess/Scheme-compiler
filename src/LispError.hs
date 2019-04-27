module LispError (
  LispError(..),
  ThrowsError,
  IOThrowsError,
  -- throwError,
  -- catchError,
  trapError,
  extractValue,
  liftThrows,
  runIOThrows,
) where

import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)
import LispVal

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
