module Gargantext.Components.GraphExplorer.Sidebar.DocList
  ( docListWrapper
  ) where

import Gargantext.Prelude

import Data.Array (concat, head)
import Data.Foldable (intercalate)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Sequence as Seq
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (Variant(..))
import Gargantext.Components.FacetsTable (DocumentsView(..), Rows(..), initialPagePath, loadPage, publicationDate)
import Gargantext.Components.GraphExplorer.Store as GraphStore
import Gargantext.Components.GraphExplorer.Types (CorpusId, DocId, GraphSideDoc(..), ListId)
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Components.RandomText (words)
import Gargantext.Components.Search (SearchQuery(..), SearchType(..))
import Gargantext.Config (defaultFrontends)
import Gargantext.Config.REST (RESTError(..))
import Gargantext.Ends (Frontends, url)
import Gargantext.Hooks.Loader (useLoaderEffect)
import Gargantext.Hooks.Session (useSession)
import Gargantext.Hooks.Sigmax.Types as SigmaxT
import Gargantext.Hooks.UpdateEffect (useUpdateEffect1')
import Gargantext.Routes as Routes
import Gargantext.Sessions (Session, sessionId)
import Gargantext.Utils ((?))
import Gargantext.Utils.Reactix as R2
import React.SyntheticEvent as SE
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer.Sidebar.DocList"

type Props =
  ( metaData :: GET.MetaData
  )

docListWrapper :: R2.Leaf Props
docListWrapper = R2.leaf docListWrapperCpt
docListWrapperCpt :: R.Component Props
docListWrapperCpt = here.component "docListWrapper" cpt where
  cpt { metaData: GET.MetaData metaData
      } _ = do
    -- | States
    -- |
    session <- useSession

    { showDoc
    , graph
    , selectedNodeIds
    } <- GraphStore.use

    graph'            <- R2.useLive' graph
    selectedNodeIds'  <- R2.useLive' selectedNodeIds

    query' /\ query <- R2.useBox' Nothing

    -- | Helpers
    -- |
    let
      nodesMap = SigmaxT.nodesGraphMap graph'

      toSearchQuery ids = SearchQuery
        { expected: SearchDoc
        , query: concat $ toQuery <$> Set.toUnfoldable ids
        }

      toQuery id = case Map.lookup id nodesMap of
        Nothing -> []
        Just n -> words n.label

    -- | Hooks
    -- |
    R.useEffect1' selectedNodeIds' $ do
      T.write_ (Just $ toSearchQuery selectedNodeIds') query

    -- | Render
    -- |
    pure $

      R.fragment
      [
        case (head metaData.corpusId) /\ query' of

          (Just corpusId) /\ (Just q') ->
            docList
            { query: q'
            , session
            , corpusId
            , listId: metaData.list.listId
            , showDoc
            , frontends: defaultFrontends
            }

          _ /\ _ ->
            B.caveat
            {}
            [
              H.text "You can link a corpus to retrieve relative documents about your selection"
            ]

      ]

-------------------------------------------------------------------

type ListProps =
  ( query           :: SearchQuery
  , corpusId        :: CorpusId
  , listId          :: ListId
  , session         :: Session
  , showDoc         :: T.Box (Maybe GraphSideDoc)
  , frontends       :: Frontends
  )

docList :: R2.Leaf ListProps
docList = R2.leaf docListCpt
docListCpt :: R.Component ListProps
docListCpt = here.component "docList" cpt where
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
  cpt { query
      , session
      , corpusId: nodeId
      , listId
      , showDoc
      , frontends
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

      callback :: Maybe GraphSideDoc -> DocId -> Effect Unit
      callback
        Nothing
        new
          = setGraphSideDoc new # Just # flip T.write_ showDoc

      callback
        (Just (GraphSideDoc { docId }))
        new
        | docId == new = T.write_ Nothing showDoc
        | otherwise    = setGraphSideDoc new # Just # flip T.write_ showDoc

      setGraphSideDoc :: DocId -> GraphSideDoc
      setGraphSideDoc docId = GraphSideDoc
        { docId
        , listId
        , corpusId: nodeId
        }

      isSelected :: Maybe GraphSideDoc -> DocumentsView -> Boolean
      isSelected
        (Just (GraphSideDoc { docId }))
        (DocumentsView { id })
          = docId == id

      isSelected
        _
        _
          = false

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
              H.text "No document found in your corpus for your selected terms"
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
              { documentView: (r :: DocumentsView)
              , callback: callback showDoc'
              , isSelected: isSelected showDoc' (r :: DocumentsView)
              , listId
              , corpusId: nodeId
              , session
              , frontends
              }
        ]


---------------------------------------------------------

type ItemProps =
  ( documentView :: DocumentsView
  , callback     :: DocId -> Effect Unit
  , isSelected   :: Boolean
  , corpusId     :: CorpusId
  , listId       :: ListId
  , session      :: Session
  , frontends    :: Frontends
  )

item :: R2.Leaf ItemProps
item = R2.leaf itemCpt
itemCpt :: R.Component ItemProps
itemCpt = here.component "item" cpt where
  cpt { documentView: dv@(DocumentsView { id, title, source })
      , callback
      , isSelected
      , listId
      , corpusId
      , session
      , frontends
      } _ = do
    -- Computed
    let
      -- Creating a href link
      route = Routes.CorpusDocument (sessionId session) corpusId listId id
      href = url frontends route

    -- Methods
    let
      onClick ::
           SE.SyntheticMouseEvent
        -> Effect Unit
      onClick event
          = R2.externalOpeningFlag event
        >>= case _ of
              true  -> R.nothing
              false -> SE.preventDefault event *> callback id

    -- Render
    pure $

      H.a
      { className: intercalate " "
          [ "graph-doc-list__item"
          , isSelected ? "graph-doc-list__item--selected" $ ""
          , "list-group-item"
          , "text-decoration-none"
          ]
      , on: { click: onClick }
      , href
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
