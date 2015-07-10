{-# LANGUAGE OverloadedStrings #-}
module Main where

import LTL
import Automaton
import Synthesis

main :: IO ()
main = do
  let ltl = ltlG (ltlAP "p" --> ltlF (ltlAP "q"))
  print $ synthesis ["p", "q"] ltl
  putStrLn "Hello, world"
