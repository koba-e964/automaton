{-# LANGUAGE OverloadedStrings #-}
module Main where

import LTL
import Automaton
import Synthesis

main :: IO ()
main = do
  print $ subformulae (ltlG (ltlAP "p" --> ltlF (ltlAP "q")))
  putStrLn "Hello, world"
