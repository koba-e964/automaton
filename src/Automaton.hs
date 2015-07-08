module Automaton where

import qualified Data.Map as Map
import Data.Word
import Data.Bits
import Data.List

-- | Positive logic (no negations)
data PosLogic s
  = PLTrue
  | PLFalse
  | PLState !s
  | PLAnd !(PosLogic s) !(PosLogic s)
  | PLOr !(PosLogic s) !(PosLogic s)
  deriving (Eq, Show)

satisfies :: Word64 -> PosLogic Int -> Bool
satisfies set = go
  where
   go p = case p of
     PLTrue -> True
     PLFalse -> False
     PLState s -> testBit set s
     PLAnd p1 p2 -> go p1 && go p2
     PLOr p1 p2 -> go p1 || go p2


-- | s : type of state, a : type of alphabet
type AlterAuto s a = Map.Map (s, a) (PosLogic s)


type NondetAuto s a = Map.Map (s, a) [s]
type DetAuto s a = Map.Map (s, a) s

-- | n : the size of universe [n] = {0, 1, ..., n - 1}
-- | bits: bit pattern which represents a subset of [n]
subsets :: Int -> Word64 -> [Word64]
subsets n bits =
  if n >= 64 then error "n must be <= 63"
  else
    go ((1 `shiftL` n - 1) .&. bits) []
  where
    go x ls
      | x == 0 = 0 : ls
      | otherwise = go ((x - 1) .&. bits) (x : ls)

listToSubset :: [Int] -> Word64
listToSubset = foldl' (\x y -> 1 `shiftL` y .|. x) 0


nfaToDfa :: Ord a => Int -> [a] -> NondetAuto Int a -> DetAuto Word64 a
nfaToDfa n alphabet nfa =
  Map.fromList [((s, a), foldl' (.|.) 0 sum) | s <- [0 .. (1 `shiftL` n) - 1], a <- alphabet,
    let sum = [ listToSubset (nfa Map.! (i, a))| i <- [0 .. n - 1], testBit s i]]


afaToNfa :: Ord a => Int -> [a] -> AlterAuto Int a -> NondetAuto Word64 a
afaToNfa n alphabet afa =
  Map.fromList [((s, a), sum) | s <- [0 .. (1 `shiftL` n) - 1], a <- alphabet,
    let sum = [t | t <- [0 .. (1 `shiftL` n) - 1], all (satisfies t) [(afa Map.! (i, a))| i <- [0 .. n - 1], testBit s i]]]

