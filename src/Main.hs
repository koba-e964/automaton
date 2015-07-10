{-# LANGUAGE OverloadedStrings #-}
module Main where

import LTL
import Automaton
import Synthesis

main :: IO ()
main = do
  let ltl = ltlG (ltlAP "p" --> ltlF (ltlAP "q"))
  let ltl2 = LTLNot (ltlAP "p") `ltlU` ltlX (ltlAP "q")
  putStrLn $ prettyPrint $ synthesis ["p", "q"] ltl2
  putStrLn "Hello, world"
