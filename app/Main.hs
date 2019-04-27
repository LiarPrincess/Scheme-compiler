module Main where

import System.IO
import Control.Monad
import Control.Monad.Except
import System.Environment
import Lib

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

-- Underscore after the name is a typical naming convention
-- for monadic functions that repeat, but do not return a value
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = nullEnv >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

runOne :: String -> IO ()
runOne input = do
  putStrLn input
  env <- nullEnv
  evalAndPrint env input
  putStrLn ""

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> do
      runOne "(a test)"
      runOne "(a (nested) test)"
      runOne "(a (dotted . list) test)"
      runOne "(a '(quoted (dotted . list)) test)"
      runOne "(a '(imbalanced parens)"
      runOne "\"'atom\""
      runOne "2"
      runOne "\"a string\""
      runOne "(+ 2 2)"
      runOne "(+ 2 (-4 1))"
      runOne "(+ 2 (- 4 1))"
      runOne "(- (+ 4 6 3) 3 5 2)"
      runOne "(< 2 3)"
      runOne "(> 2 3)"
      runOne "(>= 3 3)"
      runOne "(string=? \"test\"  \"test\")"
      runOne "(string<? \"abc\" \"bba\")"
      runOne "(if (> 2 3) \"no\" \"yes\")"
      runOne "(if (= 3 3) (+ 2 3 (- 5 1)) \"unequal\")"
      runOne "(cdr '(a simple test))"
      runOne "(car (cdr '(a simple test)))"
      runOne "(car '((this is) a test))"
      runOne "(cons '(this is) 'test)"
      runOne "(cons '(this is) '())"
      runOne "(eqv? 1 3)"
      runOne "(eqv? 3 3)"
      runOne "(eqv? 'atom 'atom)"
      runOne "(define x 3)"
      runOne "(+ x 2)"
      runOne "(+ y 2)"
      runOne "(define y 5)"
      runOne "(+ x (- y 2))"
      runOne "(define str \"A string\")"
      runOne "(< str \"The string\")"
      runOne "(string<? str \"The string\")"
    otherwise -> putStrLn "Program takes only 0 or 1 argument"
