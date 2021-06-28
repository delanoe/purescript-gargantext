module Gargantext.Components.Category.Types where

import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)
import Simple.JSON as JSON

import Gargantext.Prelude

------------------------------------------------------------------------
data Star = Star_0 | Star_1 | Star_2 | Star_3 | Star_4

stars :: Array Star
stars = [Star_0, Star_1, Star_2, Star_3, Star_4]

derive instance Generic Star _
instance Show Star where show = genericShow
instance Eq Star where eq = genericEq
instance JSON.ReadForeign Star where
  readImpl f = do
    inst <- JSON.readImpl f
    pure $ decodeStar inst
instance JSON.WriteForeign Star where writeImpl = JSON.writeImpl <<< star2score

decodeStar :: Int -> Star
decodeStar 0 = Star_0
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


clickAgain :: Star -> Star
clickAgain Star_0 = Star_1
clickAgain s      = decodeStar (star2score s - 1)


------------------------------------------------------------------------
data Category = Trash | UnRead | Checked | Topic | Favorite

categories :: Array Category
categories = [Trash, UnRead, Checked, Topic, Favorite]

derive instance Generic Category _
instance Show Category where show = genericShow
instance Eq Category where eq = genericEq
instance JSON.ReadForeign Category where
  readImpl f = do
    inst <- JSON.readImpl f
    pure $ decodeCategory inst
instance JSON.WriteForeign Category where writeImpl = JSON.writeImpl <<< cat2score

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
