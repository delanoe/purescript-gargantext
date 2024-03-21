module Gargantext.Components.PhyloExplorer.Sidebar.DocList
  ( docListWrapper
  ) where

import Gargantext.Prelude

import Data.Array as A
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.Sequence as Seq
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (Variant(..))
import Gargantext.Components.FacetsTable (DocumentsView(..), Rows(..), publicationDate, initialPageGQL, loadPageGQL)
import Gargantext.Components.PhyloExplorer.Store as PhyloStore
import Gargantext.Components.PhyloExplorer.Types (CorpusId, DocId, FrameDoc(..), ListId)
import Gargantext.Components.Search (SearchQuery(..), SearchType(..))
import Gargantext.Config (defaultFrontends)
import Gargantext.Config.REST (RESTError(..))
import Gargantext.Ends (Frontends, url)
import Gargantext.Hooks.Loader (useLoaderEffect)
import Gargantext.Hooks.Session (useSession)
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
here = R2.here "Gargantext.Components.PhyloExplorer.Sidebar.DocList"


docListWrapper :: R2.Leaf ()
docListWrapper = R2.leaf docListWrapperCpt

docListWrapperCpt :: R.Component ()
docListWrapperCpt = here.component "wrapper" cpt where
  cpt _ _ = do
    -- | States
    -- |
    session <- useSession

    store <- PhyloStore.use

    corpusId       <- R2.useLive' store.corpusId
    listId         <- R2.useLive' store.listId
    selectedTerm   <- R2.useLive' store.selectedTerm
    
    query' /\ query <- R2.useBox' Nothing

    -- | Helpers
    -- |
    let

      toSearchQuery t = SearchQuery
        { expected: SearchDoc
        , query: A.catMaybes [t]
        }

    -- | Hooks
    -- |
    R.useEffect1' selectedTerm $
      T.write_ (selectedTerm # toSearchQuery >>> Just) query

    -- | Render
    -- |
    pure $

      R.fragment
      [
        case query' of

          Nothing ->
            B.caveat
            {}
            [
              H.text "You can link a corpus to retrieve relative documents about your selection"
            ]

          Just q' ->
            docList
            { query: q'
            , session
            , corpusId
            , listId
            , frameDoc: store.frameDoc
            , frontends: defaultFrontends
            }
      ]

-------------------------------------------------------------------

type ListProps =
  ( query           :: SearchQuery
  , corpusId        :: CorpusId
  , listId          :: ListId
  , session         :: Session
  , frameDoc        :: T.Box (Maybe FrameDoc)
  , frontends       :: Frontends
  )

docList :: R2.Leaf ListProps
docList = R2.leaf docListCpt

docListCpt :: R.Component ListProps
docListCpt = here.component "main" cpt where
  -- | Helpers
  -- |
  errorHandler err = do
    here.warn2 "[docList] RESTError" err
    case err of
      ReadJSONError err' ->
        here.warn2 "[docList] ReadJSONError" $ show err'
      _ -> pure unit
  -- | Component
  -- |
  cpt { query: query@(SearchQuery { query: q' })
      , session
      , corpusId: nodeId
      , listId
      , frameDoc
      , frontends
      } _ = do
    -- | States
    -- |


    path' /\ path <- R2.useBox' $ initialPageGQL { corpusId: nodeId
                                                 , ngramsTerms: q'
                                                 , session }

    state' /\ state <-
      R2.useBox' Nothing

    rows' /\ rows <-
      R2.useBox' Nothing

    frameDoc' <-
      R2.useLive' frameDoc

    -- | Hooks
    -- |

    useLoaderEffect
      { errorHandler
      , state
      , loader: loadPageGQL
      , path: path'
      }

    -- | Effects
    -- |

    -- (on query change, reload fetched docs)
    useUpdateEffect1' query $
      T.write_ (initialPageGQL { corpusId: nodeId
                               , ngramsTerms: q'
                               , session }) path

    -- (on fetch success, extract existing docs)
    useUpdateEffect1' state' case state' of

      Nothing -> T.write_ (Just Seq.empty) rows
      Just r -> case r of
        Docs { docs } -> T.write_ (Just docs) rows
        _             -> T.write_ (Just Seq.empty) rows

    -- | Computed
    -- |
    let

      callback :: Maybe FrameDoc -> DocId -> Effect Unit
      callback
        Nothing
        new
          = setFrameDoc new # Just # flip T.write_ frameDoc

      callback
        (Just (FrameDoc { docId }))
        new
        | docId == new = T.write_ Nothing frameDoc
        | otherwise    = setFrameDoc new # Just # flip T.write_ frameDoc

      setFrameDoc :: DocId -> FrameDoc
      setFrameDoc docId = FrameDoc
        { docId
        , listId
        , corpusId: nodeId
        }

      isSelected :: Maybe FrameDoc -> DocumentsView -> Boolean
      isSelected
        (Just (FrameDoc { docId }))
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
          H.div {}
          [
            H.h6 {} [ H.text $ show (Seq.length results) <> " related documents"]
            ,
            H.ul
              { className: intercalate " "
                [ "phylo-doc-list"
                , "list-group"
                ]
              } $
              Seq.toUnfoldable $ flip Seq.map results \r ->
              
              item
              { documentView: (r :: DocumentsView)
              , callback: callback frameDoc'
              , isSelected: isSelected frameDoc' (r :: DocumentsView)
              , listId
              , corpusId: nodeId
              , session
              , frontends
              }
            ]
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
          [ "phylo-doc-list__item"
          , isSelected ? "phylo-doc-list__item--selected" $ ""
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
          { className: "phylo-doc-list__item__main" }
          [
            B.div'
            { className: "phylo-doc-list__item__title" }
            title
          ,
            B.div'
            { className: "phylo-doc-list__item__source" }
            source
          ,
            B.div'
            { className: "phylo-doc-list__item__date" } $
            publicationDate dv
          ]
        ,
          H.div
          { className: "phylo-doc-list__item__aside" }
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
