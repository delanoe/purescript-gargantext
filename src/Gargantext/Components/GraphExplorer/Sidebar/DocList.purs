module Gargantext.Components.GraphExplorer.Sidebar.DocList
  ( docList
  ) where

import Gargantext.Prelude

import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.Sequence as Seq
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (Variant(..))
import Gargantext.Components.FacetsTable (DocumentsView(..), PagePath, Rows(..), initialPagePath, loadPage, publicationDate)
import Gargantext.Components.GraphExplorer.Types (GraphSideCorpus(..), GraphSideDoc(..), DocId)
import Gargantext.Components.Search (SearchQuery)
import Gargantext.Config.REST (RESTError(..))
import Gargantext.Ends (Frontends)
import Gargantext.Hooks.Loader (useLoaderEffect)
import Gargantext.Hooks.UpdateEffect (useUpdateEffect1')
import Gargantext.Sessions (Session)
import Gargantext.Utils ((?))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer.Sidebar.DocList"

type TabsProps =
  ( frontends       :: Frontends
  , query           :: SearchQuery
  , session         :: Session
  , graphSideCorpus :: GraphSideCorpus
  , showDoc         :: T.Box (Maybe GraphSideDoc)
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
  cpt { frontends
      , query
      , session
      , graphSideCorpus: GraphSideCorpus
          { corpusId: nodeId
          , listId
          }
      , showDoc
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

      R2.fromMaybe_ rows' \results ->

        R.fragment
        [
          R2.if' (results == Seq.empty) $

            B.caveat
            {}
            [
              H.text "No docs found in your corpus for your selected terms"
            ]
        ,
          R2.if' (not $ eq results Seq.empty) $

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
