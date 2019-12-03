module Gargantext.Utils.Spec where

import Prelude
import Data.Array (index)
import Data.Foldable (all)
import Data.Maybe (Maybe(..), isJust)
import Data.String (drop, stripPrefix, Pattern(..))
import Data.Tuple (Tuple(..))
import Gargantext.Utils as U
import Gargantext.Utils.Math as UM
-- import Test.QuickCheck ((===), (/==), (<?>), Result(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck')

spec :: Spec Unit
spec =
  describe "G.Utils" do
    it "zeroPad 1 works" do
      U.zeroPad 1 0 `shouldEqual` "0"
      U.zeroPad 1 1 `shouldEqual` "1"
      U.zeroPad 1 10 `shouldEqual` "10"
    it "zeroPad 2 works" do
      U.zeroPad 2 0 `shouldEqual` "00"
      U.zeroPad 2 1 `shouldEqual` "01"
      U.zeroPad 2 10 `shouldEqual` "10"
      U.zeroPad 2 100 `shouldEqual` "100"
    it "zeroPad 3 works" do
      U.zeroPad 3 0 `shouldEqual` "000"
      U.zeroPad 3 1 `shouldEqual` "001"
      U.zeroPad 3 10 `shouldEqual` "010"
      U.zeroPad 3 99 `shouldEqual` "099"
      U.zeroPad 3 100 `shouldEqual` "100"
      U.zeroPad 3 101 `shouldEqual` "101"
      U.zeroPad 3 1000 `shouldEqual` "1000"
    it "log10 10" do
      UM.log10 10.0 `shouldEqual` 1.0
