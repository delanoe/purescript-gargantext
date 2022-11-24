-- TODO: this module should be replaced by FacetsTable
module Gargantext.Components.Category where

import Gargantext.Prelude

import Data.Array as A
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (Variant(..))
import Gargantext.Components.Category.Types (Category(..), Star(..), cat2score, categories, clickAgain, star2score, stars)
import Gargantext.Components.DocsTable.Types (DocumentsView(..), LocalCategories, LocalUserScore)
import Gargantext.Config.REST (AffRESTError)
import Gargantext.Routes (SessionRoute(NodeAPI))
import Gargantext.Sessions (Session, put)
import Gargantext.Types (NodeID, NodeType(..))
import Gargantext.Utils ((?))
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Reactix.DOM.HTML as H
import Simple.JSON as JSON

here :: R2.Here
here = R2.here "Gargantext.Components.Category"

type RatingProps =
  ( chartReload        :: T2.ReloadS
  , nodeId             :: NodeID
  , row                :: DocumentsView
  , score              :: Star
  , session            :: Session
  , setLocalCategories :: R.Setter LocalUserScore
  )

rating :: R2.Component RatingProps
rating = R.createElement ratingCpt
ratingCpt :: R.Component RatingProps
ratingCpt = here.component "rating" cpt where
  cpt { chartReload
      , nodeId
      , row: DocumentsView r
      , score
      , session
      , setLocalCategories
      } _ = do
    -- | Computed
    -- |
    let
      icon' Star_0 Star_0  = "times-circle"
      icon' _      Star_0  = "times"
      icon' c      s       = star2score c < star2score s ? "star-o" $ "star"

      variant' Star_0 Star_0 = Dark
      variant' _      Star_0 = Dark
      variant' _      _      = Dark

      className' Star_0 Star_0 = "rating-group__action"
      className' _      Star_0 = "rating-group__action"
      className' _      _      = "rating-group__star"

    -- | Behaviors
    -- |
    let
      onClick c _ = do
        let c' = score == c ? clickAgain c $ c

        setLocalCategories $ Map.insert r._id c'
        launchAff_ do
          _ <- putRating session nodeId $ RatingQuery
            { nodeIds: [r._id]
            , rating: c'
            }
          liftEffect $ T2.reload chartReload

    -- | Render
    -- |
    pure $

      H.div
      { className: "rating-group" } $
      stars <#> \s ->
        B.iconButton
        { name: icon' score s
        , callback: onClick s
        , overlay: false
        , variant: variant' score s
        , className: className' score s
        }


newtype RatingQuery =
  RatingQuery { nodeIds :: Array Int
              , rating  :: Star
              }
derive instance Generic RatingQuery _
instance JSON.WriteForeign RatingQuery where
  writeImpl (RatingQuery post) = JSON.writeImpl { ntc_nodesId: post.nodeIds
                                                , ntc_category: post.rating }

putRating :: Session -> Int -> RatingQuery -> AffRESTError (Array Int)
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
          launchAff_ $ do
            _ <- putCategories session nodeId $ CategoryQuery {nodeIds: [r._id], category: c}
            pure unit

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
derive instance Generic CategoryQuery _
instance JSON.WriteForeign CategoryQuery where
  writeImpl (CategoryQuery post) = JSON.writeImpl { ntc_nodesId: post.nodeIds
                                                  , ntc_category: post.category }

categoryRoute :: Int -> SessionRoute
categoryRoute nodeId = NodeAPI Node (Just nodeId) "category"

putCategories :: Session -> Int -> CategoryQuery -> AffRESTError (Array Int)
putCategories session nodeId = put session $ categoryRoute nodeId
