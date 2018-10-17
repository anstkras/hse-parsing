{-# LANGUAGE FlexibleInstances #-}

module Main where

import Parser
import Combinators (Result (Success, Error))

runParser :: String -> IO ()
runParser input = do
  putStrLn input
  print $ parse input
  putStrLn ""

instance {-# OVERLAPPING #-} Show a => Show (Maybe (Result a)) where
  show (Just (Success tree)) = show tree
  show (Just (Error err)) = "Syntax error: " ++ err
  show Nothing = "Empty tree"

main :: IO ()
main = do
  runParser "1-2-3"
  runParser "(((9)))"
  runParser "1*2-3/4+5"
  runParser "!"
  runParser "1 + 2"
  runParser "(9)"
  runParser "(1 + 3)"
  runParser "12 * 13 + 2345 - 123 + (125*3)"
  runParser "var = 34"
  runParser "var = qwe = (123 + asd)"
  runParser "var = -1"
  runParser "-123 + 4 * (-345)"
  runParser "var = -qwe + 0"
  runParser "-(-(-1))"
  runParser "var = 3^43"
  runParser "var^43 + 32^34^(-12*34^21) - 3^-1"
  runParser "1 + (5 + 6) + 3 + 4"
  runParser "1-2-3"
  runParser "1-(2-3)"
  runParser "-2^2"
  runParser "(-2)^2"
  runParser "x = 13;\ny = z = 42 + 6;\n777"
  runParser " (((9)))"
  runParser "a * b / c * d"
  runParser "( (   ( 9)  ))    "
  runParser " sadf    +  sfds + 43^(a   = 5);    -32^-21  ; a  "