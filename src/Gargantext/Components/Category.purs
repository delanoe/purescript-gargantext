-- TODO: this module should be replaced by FacetsTable
module Gargantext.Components.Category where

import Gargantext.Prelude (discard, map, pure, void, ($), (-), (<), (<>), (==))
import Data.Argonaut (class EncodeJson, encodeJson, jsonEmptyObject, (:=), (~>))
import Data.Array as A
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, launchAff)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Components.Category.Types
  ( Category(..), Star(..), cat2score, categories, clickAgain, star2score, stars )
import Gargantext.Components.DocsTable.Types
  ( DocumentsView(..), LocalCategories, LocalUserScore )
import Gargantext.Utils.Reactix as R2
import Gargantext.Routes (SessionRoute(NodeAPI))
import Gargantext.Sessions (Session, put)
import Gargantext.Types (NodeID, NodeType(..))

here :: R2.Here
here = R2.here "Gargantext.Components.Category"

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
ratingCpt = here.component "rating" cpt where
  cpt { nodeId, row: DocumentsView r, score, session, setLocalCategories } _ =
    pure $ H.div { className:"flex" } divs where
      divs = map (\s -> H.div { className : icon' score s
                              , on: { click: onClick s } } []) stars
      icon' Star_0 Star_0  = "fa fa-times-circle"
      icon' _ Star_0       = "fa fa-times"
      icon' c s = if star2score c < star2score s then "fa fa-star-o" else "fa fa-star"
      onClick c _ = do
        let c' = if score == c
                  then clickAgain c
                  else c

        setLocalCategories $ Map.insert r._id c'
        void $ launchAff
             $ putRating session nodeId
             $ RatingQuery { nodeIds: [r._id], rating: c' }

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
putRating session nodeId = put session $ ratingRoute where
  ratingRoute = NodeAPI Node (Just nodeId) "category"

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
carousselCpt = here.component "caroussel" cpt
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
