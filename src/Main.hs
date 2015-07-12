{-# LANGUAGE OverloadedStrings #-}
module Main where

import LTL
import Synthesis


convert :: [PropName] -> LTL -> String
convert aps ltl = afaPrettyPrint aps $ synthesis aps ltl

main :: IO ()
main = do
  let ltl = ltlG (ltlAP "p" --> ltlF (ltlAP "q"))
  let ltl2 = LTLNot (ltlAP "p") `ltlU` ltlX (ltlAP "q")
  let ltl3 = ltlG (ltlF (ltlAP "p") --> ltlAP "q")
  let aps = ["p", "q"]
  putStrLn $ convert aps ltl
  putStrLn $ convert aps ltl2
  putStrLn $ convert aps ltl3
