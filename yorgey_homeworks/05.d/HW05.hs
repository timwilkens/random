module HW05 where

import Ring
import Parser
import Data.Maybe ( listToMaybe )

data Mod5 = MkMod Integer
  deriving (Read, Eq, Show)

instance Parsable Mod5 where
  parse = listToMaybe . reads

instance Ring Mod5 where
  addId = MkMod 0
  addInv (MkMod x) = MkMod $ negate x
  mulId = MkMod 1

  add (MkMod x) (MkMod y)= MkMod $ (x + y) `mod` 5
  mul (MkMod x) (MkMod y) = MkMod $ (x * y) `mod` 5

modParsingWorks :: Bool
modParsingWorks = addId == (0 :: Integer) &&
                  (parse "MkMod 5") == Just (MkMod 5, "") &&
                  (parseRing "MkMod 4 + MkMod 2") == Just (MkMod 1) &&
                  (parseRing "MkMod 3 * MkMod 4") == Just (MkMod 2)
                   
