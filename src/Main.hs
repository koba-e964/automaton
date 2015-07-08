{-# LANGUAGE OverloadedStrings #-}
module Main where

import LTL

main :: IO ()
main = do
  print (ltlF (ltlAP "p"))
  putStrLn "Hello, world"
