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
 | LTLTrue
 deriving (Eq, Show)

ltlU :: LTL -> LTL -> LTL
ltlF, ltlG :: LTL -> LTL
ltlU = LTLUntil
ltlF = LTLUntil LTLTrue
ltlG x = LTLNot (ltlF x)

ltlAP :: PropName -> LTL
ltlAP = AtomicProp



