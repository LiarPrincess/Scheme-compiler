module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn "(a test)"
  putStrLn (myParse "(a test)")
  putStrLn ""

  putStrLn "(a (nested) test)"
  putStrLn (myParse "(a (nested) test)")
  putStrLn ""

  putStrLn "(a (dotted . list) test)"
  putStrLn (myParse "(a (dotted . list) test)")
  putStrLn ""

  putStrLn "(a '(quoted (dotted . list)) test)"
  putStrLn (myParse "(a '(quoted (dotted . list)) test)")
  putStrLn ""

  putStrLn "(a '(imbalanced parens)"
  putStrLn (myParse "(a '(imbalanced parens)")
  putStrLn ""

