module Gargantext.Components.Category.Types where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)

import Gargantext.Prelude

------------------------------------------------------------------------
data Star = Star_0 | Star_1 | Star_2 | Star_3 | Star_4

stars :: Array Star
stars = [Star_0, Star_1, Star_2, Star_3, Star_4]

derive instance genericStar :: Generic Star _

instance showStar :: Show Star where
  show = genericShow
instance eqStar :: Eq Star where
  eq = genericEq
instance decodeJsonStar :: DecodeJson Star where
  decodeJson json = do
    obj <- decodeJson json
    pure $ decodeStar obj
instance encodeJsonStar :: EncodeJson Star where
  encodeJson x    = encodeJson (star2score x)

decodeStar :: Int -> Star
decodeStar 0 = Star_1
decodeStar 1 = Star_1
decodeStar 2 = Star_2
decodeStar 3 = Star_3
decodeStar 4 = Star_4
decodeStar _ = Star_4

star2score :: Star -> Int
star2score Star_0 = 0
star2score Star_1 = 1
star2score Star_2 = 2
star2score Star_3 = 3
star2score Star_4 = 4

------------------------------------------------------------------------
data Category = Trash | UnRead | Checked | Topic | Favorite

categories :: Array Category
categories = [Trash, UnRead, Checked, Topic, Favorite]

derive instance genericFavorite :: Generic Category _
instance showCategory :: Show Category where
  show = genericShow
instance eqCategory :: Eq Category where
  eq = genericEq
instance decodeJsonCategory :: DecodeJson Category where
  decodeJson json = do
    obj <- decodeJson json
    pure $ decodeCategory obj
instance encodeJsonCategory :: EncodeJson Category where
  encodeJson cat    = encodeJson (cat2score cat)

favCategory :: Category -> Category
favCategory Favorite = Topic
favCategory _        = Favorite

trashCategory :: Category -> Category
trashCategory _     = Trash
-- TODO: ?
--trashCategory Trash = UnRead

decodeCategory :: Int -> Category
decodeCategory 0 = Trash
decodeCategory 1 = UnRead
decodeCategory 2 = Checked
decodeCategory 3 = Topic
decodeCategory 4 = Favorite
decodeCategory _ = UnRead

cat2score :: Category -> Int
cat2score Trash    = 0
cat2score UnRead   = 1
cat2score Checked  = 2
cat2score Topic    = 3
cat2score Favorite = 4
