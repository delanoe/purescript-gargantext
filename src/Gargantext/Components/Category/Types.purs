module Gargantext.Components.Category.Types where

import Data.Bounded.Generic (genericTop, genericBottom)
import Data.Enum (class Enum, class BoundedEnum, succ, pred, fromEnum, toEnumWithDefaults)
import Data.Enum.Generic (genericPred, genericSucc, genericCardinality, genericFromEnum, genericToEnum)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromMaybe)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Simple.JSON as JSON

import Gargantext.Prelude

------------------------------------------------------------------------
data Category = Trash | UnRead | Checked | Topic | Favorite | ToCite
{-
- `UnRead` is assigned initially for new docs
- After reading a doc, `Checked` should be assigned automatically
- Both `Trash` and `UnRead` map to 0 stars in the doc list
-}

categories :: Array Category
categories = [Trash, UnRead, Checked, Topic, Favorite, ToCite]

derive instance Generic Category _
instance Ord Category where compare = genericCompare
instance Enum Category where
  pred = genericPred
  succ = genericSucc
instance Bounded Category where
  bottom = genericBottom
  top = genericTop
instance BoundedEnum Category where
  cardinality = genericCardinality
  fromEnum = genericFromEnum
  toEnum = genericToEnum
instance Show Category where show = genericShow
instance Eq Category where eq = genericEq
instance JSON.ReadForeign Category where
  readImpl f = do
    inst <- JSON.readImpl f
    pure $ decodeCategory inst
instance JSON.WriteForeign Category where writeImpl = JSON.writeImpl <<< cat2score

catSucc :: Category -> Category
catSucc c = fromMaybe ToCite $ succ c

catPred :: Category -> Category
catPred c = fromMaybe Trash $ pred c

clickAgain :: Category -> Category
clickAgain _     = UnRead


-- | `categoryNextState :: current -> clicked -> new State`
categoryNextState :: Category -> Star -> Category
categoryNextState Trash Star_0 = UnRead
categoryNextState _     Star_0 = Trash
categoryNextState current clicked =
  if (cat2star current) == clicked then
    clickAgain current
  else
    star2catSimple clicked

favCategory :: Category -> Category
favCategory Favorite = Topic
favCategory _        = Favorite

trashCategory :: Category -> Category
trashCategory = const Trash

decodeCategory :: Int -> Category
decodeCategory = toEnumWithDefaults UnRead UnRead

cat2score :: Category -> Int
cat2score = fromEnum

------------------------------------------------------------------------
-- | This is just a helper to visualize categories.
data Star = Star_0 | Star_1 | Star_2 | Star_3 | Star_4

stars :: Array Star
stars = [Star_0, Star_1, Star_2, Star_3, Star_4]

derive instance Generic Star _
instance Show Star where show = genericShow
instance Eq Star where eq = genericEq
instance Ord Star where compare = genericCompare
instance Enum Star where
  pred = genericPred
  succ = genericSucc
instance Bounded Star where
  bottom = genericBottom
  top = genericTop
instance BoundedEnum Star where
  cardinality = genericCardinality
  fromEnum = genericFromEnum
  toEnum = genericToEnum



cat2star :: Category -> Star
cat2star Trash    = Star_0
cat2star UnRead   = Star_0
cat2star Checked  = Star_1
cat2star Topic    = Star_2
cat2star Favorite = Star_3
cat2star ToCite   = Star_4

-- | This is a "reverse" of `cat2star`
star2catSimple :: Star -> Category
star2catSimple Star_0 = UnRead
star2catSimple Star_1 = Checked
star2catSimple Star_2 = Topic
star2catSimple Star_3 = Favorite
star2catSimple Star_4 = ToCite
