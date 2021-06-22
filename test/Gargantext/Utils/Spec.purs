module Gargantext.Utils.Spec where

import Data.Argonaut as Argonaut
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Either (Either(..), isLeft)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Gargantext.Prelude

import Gargantext.Utils as GU
import Gargantext.Utils.Argonaut (genericEnumDecodeJson, genericEnumEncodeJson, genericSumDecodeJson, genericSumEncodeJson)
import Gargantext.Utils.Crypto as Crypto
import Gargantext.Utils.Math as GUM

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

    it "genericSumDecodeJson works" do
      let result1 = Argonaut.decodeJson =<< Argonaut.parseJson """{"Boat":{"hi":1}}"""
      result1 `shouldEqual` Right (Boat { hi: 1 })

      let result2 = Argonaut.decodeJson =<< Argonaut.parseJson """{"Gravy":"hi"}"""
      result2 `shouldEqual` Right (Gravy "hi")

      let result3 = Argonaut.decodeJson =<< Argonaut.parseJson """{"Boat":123}"""
      isLeft (result3 :: Either JsonDecodeError Fruit) `shouldEqual` true

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
      let result1 = Argonaut.decodeJson =<< Argonaut.parseJson "\"Member1\""
      result1 `shouldEqual` Right Member1

      let result2 = Argonaut.decodeJson =<< Argonaut.parseJson "\"Member2\""
      result2 `shouldEqual` Right Member2

      let result3 = Argonaut.decodeJson =<< Argonaut.parseJson "\"Failure\""
      isLeft (result3 :: Either JsonDecodeError EnumTest) `shouldEqual` true

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

------------------------------------------------------------------------
-- | Crypto Hash tests
    it "Hash String with backend works" do
      let text = "To hash with backend"
      let hashed = "8a69a94d164279af2b7d1443ce08da6184b3d7e815406076e148159c284b53c3"
                   -- ^ hash from backend with text above
      Crypto.hash text `shouldEqual` hashed

    it "Hash List with backend works" do
      let list = ["a","b"]
      let hashed = "ab19ec537f09499b26f0f62eed7aefad46ab9f498e06a7328ce8e8ef90da6d86"
                   -- ^ hash from backend with text above
      Crypto.hash list `shouldEqual` hashed

------------------------------------------------------------------------
-- | TODO property based tests
    it "Hash works with any order of list" do
      let hash1 = Crypto.hash ["a","b"]
      let hash2 = Crypto.hash ["b","a"]
      hash1 `shouldEqual` hash2




