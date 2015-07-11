module Synthesis where

import Automaton
import LTL
import Data.Bits
import Data.List (elemIndex, intercalate, nub)
import qualified Data.Map as Map
import Data.Word


data Literal = PosLit PropName | NegLit PropName

negateSub :: PosLogic LTL -> PosLogic LTL
negateSub logic = case logic of
  PLTrue -> PLFalse
  PLFalse -> PLTrue
  PLState (LTLNot s) -> PLState s
  PLState s -> PLState (LTLNot s)
  PLAnd l r -> negateSub l `PLOr` negateSub r
  PLOr l r -> negateSub l `PLAnd` negateSub r

sat :: PropName -> Word64 -> [PropName] -> Bool
sat p truth aps =
  case elemIndex p aps of
    Nothing -> False
    Just ind -> testBit truth ind
  

synthesis :: [PropName] -> LTL -> AlterAuto LTL Word64
synthesis aps ltl =
  let subform = nub $ concatMap (\x -> [x, ltlNot x]) $ subformulae ltl
      len = length aps in
  let trans = Map.fromList [((s, truth), rho s truth) | s <- subform, truth <- [0 .. shiftL 1 len - 1]]
      rho s truth = plSimpl $ case s of
        AtomicProp p -> if sat p truth aps then PLTrue else PLFalse
        LTLOr l r -> rho l truth `PLOr` rho r truth
        LTLAnd l r -> rho l truth `PLAnd` rho r truth
        LTLNot l -> negateSub (rho l truth)
        LTLUntil l r -> rho r truth `PLOr` (rho l truth `PLAnd` PLState s)
        LTLNext x -> PLState x
        LTLTrue -> PLTrue -- error "next state of true"
   in
  trans

afaPrettyPrint :: [PropName] -> AlterAuto LTL Word64 -> String
afaPrettyPrint aps auto = intercalate "\n" (map f (Map.toList auto))
  where
    f ((s, truth), target) = "(" ++ show s ++ ", " ++ showTruth truth ++ ") -> " ++ show target
    showTruth truth = "{" ++ intercalate "," [show (aps !! i) | i <- [0 .. length aps - 1], testBit truth i] ++ "}"

