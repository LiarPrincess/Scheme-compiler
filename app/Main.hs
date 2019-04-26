module Main where

import Control.Monad
import System.Environment
import Lib

run :: String -> IO ()
run input = do
  putStrLn input
  evaled <- return $ liftM show $ readExpr input >>= eval
  putStrLn $ extractValue $ trapError evaled
  putStrLn ""

main :: IO ()
main = do
  (expr:_) <- getArgs
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
