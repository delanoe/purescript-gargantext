module Gargantext.Components.Category.Types where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)

import Gargantext.Prelude

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
