module Gargantext.Utils.KarpRabin.Spec where

import Prelude
import Data.Array (index)
import Data.Foldable (all)
import Data.Maybe (Maybe(..), isJust)
import Data.String (drop, stripPrefix, Pattern(..))
import Data.Tuple (Tuple(..))
import Gargantext.Utils.KarpRabin (indicesOfAny)
-- import Test.QuickCheck ((===), (/==), (<?>), Result(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck')

validIndices :: Array String -> String -> Boolean
validIndices pats input = all validIndex (indicesOfAny pats input)
  where
    validIndex (Tuple i ps) = all validPat ps
      where
        input' = drop i input
        validPat p =
          case index pats p of
            Just pat -> isJust (stripPrefix (Pattern pat) input')
                        -- <?> (show input' <> " should start with " <> show pat)
            Nothing  -> false -- Failed "out of bounds pattern"

spec :: Spec Unit
spec =
  describe "KarpRabin" do
    it "works on a single pattern matching two times" do
      let pats   = ["ab"]
      let input  = "abcbab"
      let output = [Tuple 0 [0], Tuple 4 [0]]
      indicesOfAny pats input `shouldEqual` output

    it "works on a many unmatching patterns" do
      let pats   = ["abd","e","bac","abcbabe"]
      let input  = "abcbab"
      let output = []
      indicesOfAny pats input `shouldEqual` output

    it "works on a simple case" do
      let pats   = ["ab","cb","bc","bca"]
      let input  = "abcbab"
      let output = [Tuple 0 [0]
                   ,Tuple 1 [2]
                   ,Tuple 2 [1]
                   ,Tuple 4 [0]
                   ]
      indicesOfAny pats input `shouldEqual` output

    it "works with overlaps" do
      let pats   = ["aba"]
      let input  = "ababa"
      let output = [Tuple 0 [0]
                   ,Tuple 2 [0]
                   ]
      indicesOfAny pats input `shouldEqual` output

    it "returns valid indices" do
      validIndices ["a","ab","ba","abc","aba","abab","abcde"]
                   "ababarbabacbbababcaccacabbababa"
          `shouldEqual` true

    it "returns valid indices 2000 random samples" do
      quickCheck' 2000 validIndices
