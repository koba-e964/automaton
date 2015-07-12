{-# LANGUAGE OverloadedStrings #-}
module LTL where

import Data.Text

type PropName = Text

data LTL
 = AtomicProp PropName
 | LTLOr LTL LTL
 | LTLAnd LTL LTL
 | LTLNot LTL
 | LTLUntil LTL LTL
 | LTLNext LTL
 | LTLTrue
 deriving (Eq, Ord)
 
instance Show LTL where
  show ltl = case ltl of
    AtomicProp x -> unpack x
    LTLOr x y -> showLTLPar x ++ " \\/ " ++ showLTLPar y
    LTLAnd x y -> showLTLPar x ++ " /\\ " ++ showLTLPar y
    LTLNot x -> "~" ++ showLTLPar x
    LTLUntil LTLTrue y -> "F " ++ showLTLPar y
    LTLUntil x y -> showLTLPar x ++ " U " ++ showLTLPar y
    LTLNext x -> "X " ++ showLTLPar x
    LTLTrue -> "T"

showLTLPar :: LTL -> String
showLTLPar ltl = case ltl of
  AtomicProp x -> unpack x
  LTLTrue -> "T"
  _ -> "(" ++ show ltl ++ ")"

ltlU, (-->) :: LTL -> LTL -> LTL
ltlF, ltlG, ltlX :: LTL -> LTL
ltlU = LTLUntil
ltlF = LTLUntil LTLTrue
ltlG x = LTLNot (ltlF (LTLNot x))
ltlX = LTLNext

ltlAP :: PropName -> LTL
ltlAP = AtomicProp

a --> b = LTLOr (LTLNot a) b

subformulae :: LTL -> [LTL]
subformulae ltl = case ltl of
  AtomicProp _ -> [ltl]
  LTLOr x y -> subformulae x ++ subformulae y ++ [ltl]
  LTLAnd x y -> subformulae x ++ subformulae y ++ [ltl]
  LTLNot x -> subformulae x ++ [ltl]
  LTLUntil x y -> subformulae x ++ subformulae y ++ [ltl]
  LTLNext x -> subformulae x ++ [ltl]
  LTLTrue -> []

ltlNot :: LTL -> LTL
ltlNot (LTLNot x) = x
ltlNot x = LTLNot x