module Main where

import qualified Data.Map as Map
import System.Timeout (timeout)
import qualified Test.Framework as TF
import qualified Test.Framework.Providers.HUnit as TFH 
import qualified Test.HUnit as TH

import Automaton

main :: IO ()
main = TF.defaultMain tests


-- | Tests two values are equal. Checking will be terminated after 2000 milliseconds.
testEq :: (Eq a, Show a) => String -> a -> a -> TF.Test
testEq msg actual expected = TFH.testCase msg $ do
  result <- timeout 2000000 $ TH.assertEqual msg expected actual
  case result of
    Nothing -> fail "time limit exceeded (2000ms)"
    _       -> return ()


tests :: [TF.Test]
tests =
  [ testNfaToDfa
  
  ]


testNfaToDfa :: TF.Test
testNfaToDfa = TF.testGroup "NFA -> DFA"
  [ testEq "0(self-loop) -a-> 1" (nfaToDfa 2 "ab" (Map.fromList [((0, 'a'), [0,1]), ((0, 'b'), [0]), ((1, 'a'), []), ((1, 'b'), [])]))
     (Map.fromList [((0, 'a'), 0), ((0, 'b'), 0), ((1, 'a'), 3), ((1, 'b'), 1), ((2, 'a'), 0), ((2, 'b'), 0), ((3, 'a'), 3), ((3, 'b'), 1)])
  ]
