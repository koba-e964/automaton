{-# LANGUAGE OverloadedStrings #-}
module Main where

import LTL
import Automaton

main :: IO ()
main = do
  print (ltlF (ltlAP "p"))
  putStrLn "Hello, world"
