module Gargantext.Data.Spec where

import Prelude
import Data.Array (index)
import Data.Foldable (all)
import Data.Maybe (Maybe(..), isJust)
import Data.String (drop, stripPrefix, Pattern(..))
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck')

import Gargantext.Data.Array as GDA

spec :: Spec Unit
spec =
  describe "G.D.Array" do
    it "swap works" do
      GDA.swap 1 0 [0, 1, 2] `shouldEqual` [1, 0, 2]
      GDA.swap 1 2 [0, 1, 2] `shouldEqual` [0, 2, 1]
    it "slidingWindow works" do
      GDA.slidingWindow [1, 2, 3, 4, 5] 2 `shouldEqual` [[1, 2], [2, 3], [3, 4], [4, 5]]
