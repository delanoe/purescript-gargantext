-- TODO: this module should be replaced by FacetsTable
module Gargantext.Components.Category where

import Gargantext.Prelude

import Data.Array as A
import Data.Enum (fromEnum)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (Variant(..))
import Gargantext.Components.Category.Types (Category(..), Star(..), cat2score, cat2star, categories, categoryNextState, decodeCategory, stars)
import Gargantext.Components.DocsTable.Types (DocumentsView(..), LocalCategories)
import Gargantext.Components.GraphQL.Context (NodeContext)
import Gargantext.Components.GraphQL.Endpoints (getNodeContext, updateNodeContextCategory)
import Gargantext.Config.REST (AffRESTError, RESTError(..))
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (SessionRoute(NodeAPI))
import Gargantext.Sessions (Session, put)
import Gargantext.Types (NodeID, NodeType(..))
import Gargantext.Utils ((?))
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Reactix.DOM.HTML as H
import Simple.JSON as JSON
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Category"

type RatingProps =
  ( chartReload        :: T2.ReloadS
  , nodeId             :: NodeID
  , row                :: DocumentsView
  , score              :: Category
  , session            :: Session
  -- , setLocalCategories :: R.Setter LocalCategories
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
      -- , setLocalCategories
      } _ = do
    pure $ renderRatingSimple { docId: r._id
                              , corpusId: nodeId
                              , category: score
                              , session } []

    -- -- | Behaviors
    -- -- |
    -- let
    --   onClick c _ = do
    --     let c' = score == c ? clickAgain c $ c

    --     setLocalCategories $ Map.insert r._id c'
    --     launchAff_ do
    --       _ <- putRating session nodeId $ RatingQuery
    --         { nodeIds: [r._id]
    --         , rating: c'
    --         }
    --       liftEffect $ T2.reload chartReload

    -- -- | Render
    -- -- |
    -- pure $

    --   H.div
    --   { className: "rating-group" } $
    --   stars <#> \s ->
    --     B.iconButton
    --     { name: ratingIcon score s
    --     , callback: onClick s
    --     , overlay: false
    --     , variant: ratingVariant score s
    --     , className: ratingClassName score s
    --     }

ratingIcon :: Category -> Star -> String
ratingIcon Trash  Star_0  = "recycle"
ratingIcon _      Star_0  = "trash"
ratingIcon c      s       = fromEnum (cat2star c) < fromEnum s ? "star-o" $ "star"

ratingVariant :: Star -> Star -> Variant
ratingVariant Star_0 Star_0 = Dark
ratingVariant _      Star_0 = Dark
ratingVariant _      _      = Dark

ratingClassName :: Star -> Star -> String
ratingClassName Star_0 Star_0 = "rating-group__action"
ratingClassName _      Star_0 = "rating-group__action"
ratingClassName _      _      = "rating-group__star"


------------------------------------------------

type RatingSimpleLoaderProps =
  ( docId    :: NodeID
  , corpusId :: NodeID
  , session  :: Session
)

ratingSimpleLoader :: R2.Component RatingSimpleLoaderProps
ratingSimpleLoader = R.createElement ratingSimpleLoaderCpt
ratingSimpleLoaderCpt :: R.Component RatingSimpleLoaderProps
ratingSimpleLoaderCpt = here.component "ratingSimpleLoader" cpt where
  cpt { docId
      , corpusId
      , session
      } _ = do
    useLoader { errorHandler
              , loader: loadDocumentContext session
              , path: { docId, corpusId }
              , render: \{ nc_category } -> do
                  let category = fromMaybe UnRead $ decodeCategory <$> nc_category
                  renderRatingSimple { docId
                                     , corpusId
                                     , category
                                     , session } [] }
    where
      errorHandler err = do
        here.warn2 "[pageLayout] RESTError" err
        case err of
          ReadJSONError err' -> here.warn2 "[pageLayout] ReadJSONError" $ show err'
          _ -> pure unit

type ContextParams =
  ( docId    :: NodeID
  , corpusId :: NodeID )

loadDocumentContext :: Session -> Record ContextParams -> AffRESTError NodeContext
loadDocumentContext session { docId, corpusId } = getNodeContext session docId corpusId

type RenderRatingSimpleProps =
  ( docId    :: NodeID
  , corpusId :: NodeID
  , category :: Category
  , session  :: Session )

renderRatingSimple :: R2.Component RenderRatingSimpleProps
renderRatingSimple = R.createElement renderRatingSimpleCpt
renderRatingSimpleCpt :: R.Component RenderRatingSimpleProps
renderRatingSimpleCpt = here.component "renderRatingSimple" cpt where
  cpt { docId
      , corpusId
      , category
      , session
      } _ = do
    categoryS <- T.useBox category

    pure $ ratingSimple { docId
                        , corpusId
                        , category: categoryS
                        , session } []

type RatingSimpleProps =
  ( docId    :: NodeID
  , corpusId :: NodeID
  , category :: T.Box Category
  , session  :: Session )

ratingSimple :: R2.Component RatingSimpleProps
ratingSimple = R.createElement ratingSimpleCpt
ratingSimpleCpt :: R.Component RatingSimpleProps
ratingSimpleCpt = here.component "ratingSimple" cpt where
  cpt { docId
      , corpusId
      , category
      , session
      } _ = do
    category' <- T.useLive T.unequal category
    let star' = cat2star category'

    let
      onClick c _ = do
        -- let c' = score' == c ? clickAgain c $ c
        let c' = categoryNextState category' c

        -- setLocalCategories $ Map.insert r._id c'
        launchAff_ do
          _ <- updateNodeContextCategory session docId corpusId $ cat2score c'
          liftEffect $ T.write_ c' category
          pure unit

    pure $
      H.div
      { className: "rating-group" } $
      stars <#> \s ->
        B.iconButton
        { name: ratingIcon category' s
        , callback: onClick s
        , overlay: false
        , variant: ratingVariant star' s
        , className: ratingClassName star' s
        }


newtype RatingQuery =
  RatingQuery { nodeIds :: Array Int
              , rating  :: Category
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
      pure $ H.div {className:"d-flex"} divs
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

    icon' ToCite false = "quote-left-o"
    icon' ToCite true = "quote-left"

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
