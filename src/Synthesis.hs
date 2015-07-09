module Synthesis where

import Automaton
import LTL
import Data.Bits
import Data.Word

synthesis :: [PropName] -> LTL -> AlterAuto LTL Word64
synthesis = undefined