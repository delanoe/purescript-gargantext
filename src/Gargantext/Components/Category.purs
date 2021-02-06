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

import Gargantext.Prelude

import Gargantext.Components.Category.Types
import Gargantext.Components.DocsTable.Types (DocumentsView(..), LocalCategories, LocalUserScore)
import Gargantext.Ends (Frontends, url)
import Gargantext.Hooks.Loader (useLoaderWithCacheAPI, HashedResponse(..))
import Gargantext.Utils.Reactix as R2
import Gargantext.Routes as Routes
import Gargantext.Routes (SessionRoute(NodeAPI))
import Gargantext.Sessions (Session, sessionId, get, delete, put)
import Gargantext.Types (NodeID, NodeType(..), OrderBy(..), TableResult, TabType, showTabType')
import Gargantext.Utils.CacheAPI as GUC

thisModule :: String
thisModule = "Gargantext.Components.Category"

------------------------------------------------------------------------
type RatingProps =
  ( score              :: Star
  , nodeId             :: NodeID
  , row                :: DocumentsView
  , session            :: Session
  , setLocalCategories :: R.Setter LocalUserScore
  )

rating :: R2.Component RatingProps
rating = R.createElement ratingCpt

ratingCpt :: R.Component RatingProps
ratingCpt = R.hooksComponentWithModule thisModule "rating" cpt
  where
    cpt { score, nodeId, row: DocumentsView r, session, setLocalCategories } _ = do
      pure $ H.div {className:"flex"} divs
        where
          divs = map (\s -> H.div { className : icon score s
                                  , on: {click: onClick score s}
                                  } []) stars

          icon Star_0 Star_0  = "fa fa-times-circle"
          icon _ Star_0       = "fa fa-times"
          icon c s            = if star2score c < star2score s
                                  then "fa fa-star-o"
                                  else "fa fa-star"

          onClick score c = \_-> do
            let c' = if score == Star_0 
                       && c == Star_0
                     then Star_1
                     else c

            setLocalCategories $ Map.insert r._id c'
            void $ launchAff
                 $ putRating session nodeId
                 $ RatingQuery {nodeIds: [r._id], rating: c'}

newtype RatingQuery =
  RatingQuery { nodeIds :: Array Int
              , rating  :: Star
              }

instance encodeJsonRatingQuery :: EncodeJson RatingQuery where
  encodeJson (RatingQuery post) =
    "ntc_nodesId" := post.nodeIds
    ~> "ntc_category" := encodeJson post.rating
    ~> jsonEmptyObject

putRating :: Session -> Int -> RatingQuery -> Aff (Array Int)
putRating session nodeId = put session $ ratingRoute nodeId
  where
    ratingRoute :: Int -> SessionRoute
    ratingRoute nodeId = NodeAPI Node (Just nodeId) "category"



------------------------------------------------------------------------
type CarousselProps =
  ( category           :: Category
  , nodeId             :: NodeID
  , row                :: DocumentsView
  , session            :: Session
  , setLocalCategories :: R.Setter LocalCategories
  )


caroussel :: R2.Component CarousselProps
caroussel = R.createElement carousselCpt

carousselCpt :: R.Component CarousselProps
carousselCpt = R.hooksComponentWithModule thisModule "caroussel" cpt
  where
    cpt { category, nodeId, row: DocumentsView r, session, setLocalCategories } _ = do
      pure $ H.div {className:"flex"} divs
      where
        divs = map (\c -> if category == c
                            then
                              H.div { className : icon c (category == c) } []

                            else
                              H.div { className : icon c (category == c)
                                , on: { click: onClick c}
                                } []
                        ) (caroussel' category)

        caroussel' :: Category -> Array Category
        caroussel' Trash = A.take 2 categories
        caroussel' c   = A.take 3 $ A.drop (cat2score c - 1 ) categories

        onClick c = \_-> do
          setLocalCategories $ Map.insert r._id c
          void $ launchAff
               $ putCategories session nodeId
               $ CategoryQuery {nodeIds: [r._id], category: c}

icon :: Category -> Boolean -> String
icon cat b = btn b $ "fa fa-" <> (color $ size b $ icon' cat b)
  where
    icon' :: Category -> Boolean -> String
    icon' Trash   false = "times"
    icon' Trash   true  = "times-circle"

    icon' UnRead  false = "question"
    icon' UnRead  true  = "question-circle"

    icon' Checked false = "check"
    icon' Checked true  = "check-circle"

    icon' Topic  false = "star-o"
    icon' Topic  true  = "star"

    icon' Favorite false = "heart-o"
    icon' Favorite true = "heart"

    size :: Boolean -> String -> String
    size true  s = s <> " btn-lg"
    size false s = s <> " btn-sm"

    color :: String -> String
    color x = x <> " text-primary"

    btn :: Boolean -> String -> String
    btn true s = s
    btn false s = "btn " <> s

-------------------------------------------------------------------------
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
