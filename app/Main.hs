module Main where

import System.IO
import Control.Monad
import System.Environment
import Lib

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

-- Underscore after the name is a typical naming convention
-- for monadic functions that repeat, but do not return a value
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint

run :: String -> IO ()
run input = do
  putStrLn input
  evaled <- return $ liftM show $ readExpr input >>= eval
  putStrLn $ extractValue $ trapError evaled
  putStrLn ""

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> do
      run "(a test)"
      run "(a (nested) test)"
      run "(a (dotted . list) test)"
      run "(a '(quoted (dotted . list)) test)"
      run "(a '(imbalanced parens)"
      run "\"'atom\""
      run "2"
      run "\"a string\""
      run "(+ 2 2)"
      run "(+ 2 (-4 1))"
      run "(+ 2 (- 4 1))"
      run "(- (+ 4 6 3) 3 5 2)"
      run "(< 2 3)"
      run "(> 2 3)"
      run "(>= 3 3)"
      run "(string=? \"test\"  \"test\")"
      run "(string<? \"abc\" \"bba\")"
      run "(if (> 2 3) \"no\" \"yes\")"
      run "(if (= 3 3) (+ 2 3 (- 5 1)) \"unequal\")"
      run "(cdr '(a simple test))"
      run "(car (cdr '(a simple test)))"
      run "(car '((this is) a test))"
      run "(cons '(this is) 'test)"
      run "(cons '(this is) '())"
      run "(eqv? 1 3)"
      run "(eqv? 3 3)"
      run "(eqv? 'atom 'atom)"
    otherwise -> putStrLn "Program takes only 0 or 1 argument"
