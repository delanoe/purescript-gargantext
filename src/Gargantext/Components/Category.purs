-- TODO: this module should be replaced by FacetsTable
module Gargantext.Components.Category where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.:), (:=), (~>), encodeJson)
import Data.Array as A
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens ((^.))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.List as L
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Ord.Down (Down(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String as Str
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\))
import DOM.Simple.Event as DE
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Reactix as R
import Reactix.DOM.HTML as H
------------------------------------------------------------------------
import Gargantext.Prelude

import Gargantext.Ends (Frontends, url)
import Gargantext.Hooks.Loader (useLoaderWithCacheAPI, HashedResponse(..))
import Gargantext.Utils.List (sortWith) as L
import Gargantext.Utils.Reactix as R2
import Gargantext.Routes as Routes
import Gargantext.Routes (SessionRoute(NodeAPI))
import Gargantext.Sessions (Session, sessionId, get, delete, put)
import Gargantext.Types (NodeType(..), OrderBy(..), TableResult, TabType, showTabType')
import Gargantext.Utils.CacheAPI as GUC
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

-- caroussel :: Category -> R.Element
caroussel session nodeId setLocalCategories r cat = H.div {className:"flex"} divs
  where
    divs = map (\c -> if cat == c
                        then
                          H.div { className : icon c (cat == c) } []

                        else
                          H.div { className : icon c (cat == c)
                            , on: { click: onClick c}
                             } []
                    ) (caroussel' cat)

    caroussel' :: Category -> Array Category
    caroussel' Trash = A.take 2 categories
    caroussel' c   = A.take 3 $ A.drop (cat2score c - 1 ) categories

    onClick c = \_-> do
      setLocalCategories $ Map.insert r._id c
      void $ launchAff $ putCategories session nodeId $ CategoryQuery {nodeIds: [r._id], category: c}

icon :: Category -> Boolean -> String
icon cat b = btn b $ "glyphicon glyphicon-" <> (color $ size b $ icon' cat b)
  where
    icon' :: Category -> Boolean -> String
    icon' Trash   false = "remove"
    icon' Trash   true  = "remove-sign"

    icon' UnRead  true  = "question-sign"
    icon' UnRead  false = "question-sign"

    icon' Checked true  = "ok-sign"
    icon' Checked false = "ok"

    icon' Topic  true  = "star"
    icon' Topic  false = "star-empty"

    icon' Favorite true = "heart"
    icon' Favorite false = "heart-empty"

    size :: Boolean -> String -> String
    size true  s = s <> " btn-lg"
    size false s = s <> " btn-xs"

    color :: String -> String
    color x = x <> " text-primary"

    btn :: Boolean -> String -> String
    btn true s = s
    btn false s = "btn " <> s


newtype CategoryQuery = CategoryQuery {
    nodeIds :: Array Int
  , category :: Category
  }

instance encodeJsonCategoryQuery :: EncodeJson CategoryQuery where
  encodeJson (CategoryQuery post) =
    "ntc_nodesId" := post.nodeIds
    ~> "ntc_category" := encodeJson post.category
    ~> jsonEmptyObject

categoryRoute :: Int -> SessionRoute
categoryRoute nodeId = NodeAPI Node (Just nodeId) "category"

putCategories :: Session -> Int -> CategoryQuery -> Aff (Array Int)
putCategories session nodeId = put session $ categoryRoute nodeId
