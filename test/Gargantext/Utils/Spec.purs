module Gargantext.Utils.Spec where

import Prelude

import Data.Argonaut as Argonaut
import Data.Either (Either(..), isLeft)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Gargantext.Utils as GU
import Gargantext.Utils.Argonaut (genericEnumDecodeJson, genericEnumEncodeJson, genericSumDecodeJson, genericSumEncodeJson)
import Gargantext.Utils.Crypto as GUC
import Gargantext.Utils.Math as GUM
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

data Fruit
  = Boat { hi :: Int }
  | Gravy String
  | Pork Int

derive instance eqFruit :: Eq Fruit
derive instance genericFruit :: Generic Fruit _
instance showFruit :: Show Fruit where
  show = genericShow
instance decodeJsonFruit :: Argonaut.DecodeJson Fruit where
  decodeJson = genericSumDecodeJson
instance encodeJsonFruit :: Argonaut.EncodeJson Fruit where
  encodeJson = genericSumEncodeJson

data EnumTest
  = Member1
  | Member2
  | Member3

derive instance eqEnumTest :: Eq EnumTest
derive instance genericEnumTest :: Generic EnumTest _
instance showEnumTest :: Show EnumTest where
  show = genericShow
instance decodeJsonEnumTest :: Argonaut.DecodeJson EnumTest where
  decodeJson = genericEnumDecodeJson
instance encodeJsonEnumTest :: Argonaut.EncodeJson EnumTest where
  encodeJson = genericEnumEncodeJson

spec :: Spec Unit
spec =
  describe "G.Utils" do
    it "zeroPad 1 works" do
      GU.zeroPad 1 0 `shouldEqual` "0"
      GU.zeroPad 1 1 `shouldEqual` "1"
      GU.zeroPad 1 10 `shouldEqual` "10"
    it "zeroPad 2 works" do
      GU.zeroPad 2 0 `shouldEqual` "00"
      GU.zeroPad 2 1 `shouldEqual` "01"
      GU.zeroPad 2 10 `shouldEqual` "10"
      GU.zeroPad 2 100 `shouldEqual` "100"
    it "zeroPad 3 works" do
      GU.zeroPad 3 0 `shouldEqual` "000"
      GU.zeroPad 3 1 `shouldEqual` "001"
      GU.zeroPad 3 10 `shouldEqual` "010"
      GU.zeroPad 3 99 `shouldEqual` "099"
      GU.zeroPad 3 100 `shouldEqual` "100"
      GU.zeroPad 3 101 `shouldEqual` "101"
      GU.zeroPad 3 1000 `shouldEqual` "1000"
    it "log10 10" do
      GUM.log10 10.0 `shouldEqual` 1.0
    it "md5 works" do
      let text = "The quick brown fox jumps over the lazy dog"
      let textMd5 = "9e107d9d372bb6826bd81d3542a419d6"
      GUC.md5 text `shouldEqual` textMd5

    it "genericSumDecodeJson works" do
      let result1 = Argonaut.decodeJson =<< Argonaut.jsonParser """{"Boat":{"hi":1}}"""
      result1 `shouldEqual` Right (Boat { hi: 1 })

      let result2 = Argonaut.decodeJson =<< Argonaut.jsonParser """{"Gravy":"hi"}"""
      result2 `shouldEqual` Right (Gravy "hi")

      let result3 = Argonaut.decodeJson =<< Argonaut.jsonParser """{"Boat":123}"""
      isLeft (result3 :: Either String Fruit) `shouldEqual` true

    it "genericSumEncodeJson works and loops back with decode" do
      let input1 = Boat { hi: 1 }
      let result1 = Argonaut.encodeJson input1
      let result1' = Argonaut.decodeJson result1
      Argonaut.stringify result1 `shouldEqual` """{"Boat":{"hi":1}}"""
      result1' `shouldEqual` Right input1

      let input2 = Gravy "hi"
      let result2 = Argonaut.encodeJson input2
      let result2' = Argonaut.decodeJson result2
      Argonaut.stringify result2 `shouldEqual` """{"Gravy":"hi"}"""
      result2' `shouldEqual` Right input2

    it "genericEnumDecodeJson works" do
      let result1 = Argonaut.decodeJson =<< Argonaut.jsonParser "\"Member1\""
      result1 `shouldEqual` Right Member1

      let result2 = Argonaut.decodeJson =<< Argonaut.jsonParser "\"Member2\""
      result2 `shouldEqual` Right Member2

      let result3 = Argonaut.decodeJson =<< Argonaut.jsonParser "\"Failure\""
      isLeft (result3 :: Either String EnumTest) `shouldEqual` true

    it "genericSumEncodeJson works and loops back with decode" do
      let input1 = Member1
      let result1 = Argonaut.encodeJson input1
      let result1' = Argonaut.decodeJson result1
      Argonaut.stringify result1 `shouldEqual` "\"Member1\""
      result1' `shouldEqual` Right input1

      let input2 = Member2
      let result2 = Argonaut.encodeJson input2
      let result2' = Argonaut.decodeJson result2
      Argonaut.stringify result2 `shouldEqual` "\"Member2\""
      result2' `shouldEqual` Right input2
