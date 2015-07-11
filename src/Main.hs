{-# LANGUAGE OverloadedStrings #-}
module Main where

import LTL
import Automaton
import Synthesis

main :: IO ()
main = do
  let ltl = ltlG (ltlAP "p" --> ltlF (ltlAP "q"))
  let ltl2 = LTLNot (ltlAP "p") `ltlU` ltlX (ltlAP "q")
  let aps = ["p", "q"]
  putStrLn $ afaPrettyPrint aps $ synthesis aps ltl2
  putStrLn "Hello, world"
