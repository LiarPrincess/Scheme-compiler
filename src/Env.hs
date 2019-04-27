module Env (
  Env,
  nullEnv,
  isBound,
  getVar,
  setVar,
  defineVar,
  bindVars
) where

import Data.IORef
import Control.Monad
import Control.Monad.Except
import LispVal
import LispError

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
