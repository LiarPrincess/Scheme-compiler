module Main where

import Lib
import System.Environment

run :: String -> IO ()
run input = do
  putStrLn input
  putStrLn $ (show . eval . readExpr) $ input
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
