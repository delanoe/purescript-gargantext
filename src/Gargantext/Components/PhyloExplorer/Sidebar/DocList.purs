module Gargantext.Components.PhyloExplorer.Sidebar.DocList
  where
{-

import Gargantext.Prelude

import Control.Parallel (parTraverse)
import Data.Array (concat, head, last, mapWithIndex)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Foldable as F
import Data.Int (fromString)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Sequence as Seq
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Gargantext.Components.App.Store as AppStore
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), Variant(..))
import Gargantext.Components.GraphExplorer.Sidebar.DocList (docList)
import Gargantext.Components.GraphExplorer.Sidebar.Legend as Legend
import Gargantext.Components.GraphExplorer.Store as GraphStore
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Components.Lang (Lang(..))
import Gargantext.Components.NgramsTable.Core as NTC
import Gargantext.Components.PhyloExplorer.Store as PhyloStore
import Gargantext.Components.RandomText (words)
import Gargantext.Components.Search (SearchQuery(..), SearchType(..))
import Gargantext.Config (defaultFrontends)
import Gargantext.Config.REST (AffRESTError)
import Gargantext.Data.Array (mapMaybe)
import Gargantext.Ends (Frontends)
import Gargantext.Hooks.Session (useSession)
import Gargantext.Hooks.Sigmax.Types as SigmaxT
import Gargantext.Sessions (Session)
import Gargantext.Types (CTabNgramType, FrontendError(..), NodeID, TabSubType(..), TabType(..), TermList(..), modeTabType)
import Gargantext.Utils (getter, nbsp)
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Partial.Unsafe (unsafePartial)
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.PhyloExplorer.Sidebar.DocList"

docListWrapper :: R2.Leaf ()
docListWrapper = R2.leaf docListWrapperCpt

docListWrapperCpt :: R.Memo ()
docListWrapperCpt = R.memo' $ here.component "wrapper" cpt where
  cpt _ _ = do
    -- | States
    -- |
    store <- PhyloStore.use

    extractedTerms <- R2.useLive' store.extractedTerms

    query' /\ query <- R2.useBox' Nothing

    -- | Helpers
    -- |
    let
      toSearchQuery ids = SearchQuery
        { expected: SearchDoc
        , query: map (getter _.label) ids
        }

      searchQuery
        = extractedTerms
        # toSearchQuery
        # Just
        # flip T.write_ query

    -- | Render
    --
    pure $

      docList
      { searchQuery }

--------------------------------------------------------------

type ListProps =
  ( searchQuery :: SearchQuery
  )

docList :: R2.Leaf TabsProps
docList = R2.leaf docListCpt

docListCpt :: R.Component TabsProps
docListCpt = here.component "main" cpt where
  -- | Helpers
  -- |
  errorHandler err = do
    here.warn2 "[pageLayout] RESTError" err
    case err of
      ReadJSONError err' ->
        here.warn2 "[pageLayout] ReadJSONError" $ show err'
      _ -> pure unit
  -- | Component
  -- |
  cpt { searchQuery
      } _ = do
    -- | States
    -- |

    path' /\ path
      <- R2.useBox' $ initialPagePath { nodeId, listId, query, session }

    state' /\ state <-
      R2.useBox' Nothing

    rows' /\ rows <-
      R2.useBox' Nothing

    showDoc' <-
      R2.useLive' showDoc

    -- | Hooks
    -- |

    useLoaderEffect
      { errorHandler
      , state
      , loader: loadPage
      , path: path'
      }

    -- | Effects
    -- |

    -- (on query change, reload fetched docs)
    useUpdateEffect1' query $
      flip T.write_ path $ initialPagePath { nodeId, listId, query, session }

    -- (on fetch success, extract existing docs)
    useUpdateEffect1' state' case state' of
      Nothing -> T.write_ (Just Seq.empty) rows
      Just r -> case r of
        Docs { docs } -> T.write_ (Just docs) rows
        _             -> T.write_ (Just Seq.empty) rows

    -- | Computed
    -- |
    let

      -- callback :: Maybe GraphSideDoc -> DocId -> Effect Unit
      -- callback
      --   Nothing
      --   new
      --     = setGraphSideDoc new # Just # flip T.write_ showDoc

      -- callback
      --   (Just (GraphSideDoc { docId }))
      --   new
      --   | docId == new = T.write_ Nothing showDoc
      --   | otherwise    = setGraphSideDoc new # Just # flip T.write_ showDoc

      -- setGraphSideDoc :: DocId -> GraphSideDoc
      -- setGraphSideDoc docId = GraphSideDoc
      --   { docId
      --   , listId
      --   , corpusId: nodeId
      --   }

      -- isSelected :: Maybe GraphSideDoc -> DocumentsView -> Boolean
      -- isSelected
      --   (Just (GraphSideDoc { docId }))
      --   (DocumentsView { id })
      --     = docId == id

      -- isSelected
      --   _
      --   _
      --     = false

    -- | Render
    -- |
    pure $

      R2.fromMaybe rows' \results ->

        R.fragment
        [
          R2.when (results == Seq.empty) $

            B.caveat
            {}
            [
              H.text "No docs found in your corpus for your selected terms"
            ]
        ,
          R2.when (not $ eq results Seq.empty) $

            H.ul
            { className: intercalate " "
                [ "graph-doc-list"
                , "list-group"
                ]
            } $
            Seq.toUnfoldable $ flip Seq.map results \r ->

              item
              { frontends
              , path: path'
              , session
              , documentView: (r :: DocumentsView)
              , callback: callback showDoc'
              , isSelected: isSelected showDoc' (r :: DocumentsView)
              }
        ]


---------------------------------------------------------

type ItemProps =
  ( documentView :: DocumentsView
  , frontends    :: Frontends
  , session      :: Session
  , path         :: PagePath
  , callback     :: DocId -> Effect Unit
  , isSelected   :: Boolean
  )

item :: R2.Leaf ItemProps
item = R2.leaf itemCpt

itemCpt :: R.Component ItemProps
itemCpt = here.component "item" cpt where
  cpt { documentView: dv@(DocumentsView { id, title, source })
      , callback
      , isSelected
      -- , frontends
      -- , path
      -- , session
      } _ = do
    -- Computed
    -- let
      -- Creating a href link
      -- documentUrl id' { listId, nodeId } =
      --   url frontends $ Routes.CorpusDocument (sessionId session) nodeId listId id'


    -- Render
    pure $

      H.div
      { className: intercalate " "
          [ "graph-doc-list__item"
          , isSelected ? "graph-doc-list__item--selected" $ ""
          , "list-group-item"
          ]
      , on: { click: \_ -> callback id }
      }
      [
        B.ripple
        { variant: Dark }
        [
          H.div
          { className: "graph-doc-list__item__main" }
          [
            B.div'
            { className: "graph-doc-list__item__title" }
            title
          ,
            B.div'
            { className: "graph-doc-list__item__source" }
            source
          ,
            B.div'
            { className: "graph-doc-list__item__date" } $
            publicationDate dv
          ]
        ,
          H.div
          { className: "graph-doc-list__item__aside" }
          [
            B.icon
            { name: "eye-slash"
            , className: intercalate " "
                [ "text-info"
                , isSelected ? "visible" $ "hidden"
                ]
            }
          ]
        ]
      ]
