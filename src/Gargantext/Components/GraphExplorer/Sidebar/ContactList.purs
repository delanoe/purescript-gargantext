module Gargantext.Components.GraphExplorer.Sidebar.ContactList
  ( contactListWrapper
  ) where

import Gargantext.Prelude

import Data.Array (concat, head)
import Data.Foldable (intercalate)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Sequence as Seq
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.FacetsTable (ContactsView(..), Rows(..), initialPagePath, loadPage)
import Gargantext.Components.GraphExplorer.Store as GraphStore
import Gargantext.Components.GraphExplorer.Types (CorpusId, ListId)
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Components.RandomText (words)
import Gargantext.Components.Search (HyperdataRowContact(..), SearchQuery(..), SearchType(..))
import Gargantext.Config (defaultFrontends)
import Gargantext.Config.REST (RESTError(..))
import Gargantext.Ends (Frontends, url)
import Gargantext.Hooks.Loader (useLoaderEffect)
import Gargantext.Hooks.Session (useSession)
import Gargantext.Hooks.Sigmax.Types as SigmaxT
import Gargantext.Hooks.UpdateEffect (useUpdateEffect1')
import Gargantext.Routes as Routes
import Gargantext.Sessions (Session, sessionId)
import Gargantext.Utils (nbsp)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer.Sidebar.ContactList"

type Props =
  ( metaData :: GET.MetaData
  )

contactListWrapper :: R2.Leaf Props
contactListWrapper = R2.leaf contactListWrapperCpt

contactListWrapperCpt :: R.Component Props
contactListWrapperCpt = here.component "wrapper" cpt where
  cpt { metaData: GET.MetaData metaData
      } _ = do
    -- | States
    -- |
    session <- useSession

    { graph
    , selectedNodeIds
    } <- GraphStore.use

    graph'            <- R2.useLive' graph
    selectedNodeIds'  <- R2.useLive' selectedNodeIds

    query' /\ query <- R2.useBox' Nothing

    -- | Helpers
    -- |
    let
      frontends = defaultFrontends

      nodesMap = SigmaxT.nodesGraphMap graph'

      toSearchQuery ids = SearchQuery
        { expected: SearchContact
        , query: concat $ toQuery <$> Set.toUnfoldable ids
        }

      toQuery id = case Map.lookup id nodesMap of
        Nothing -> []
        Just n -> words n.label

    -- | Hooks
    -- |
    R.useEffect1' selectedNodeIds' $
      T.write_ (selectedNodeIds' # toSearchQuery >>> Just) query

    -- | Render
    -- |
    pure $

      R.fragment
      [
        case (head metaData.corpusId) /\ query' of

          (Just corpusId) /\ (Just q') ->
            contactList
            { frontends
            , query: q'
            , session
            , corpusId
            , listId: metaData.list.listId
            }

          _ /\ _ ->
            B.caveat
            {}
            [
              H.text "You can link an annuaire to retrieve relative contacts about your selection"
            ]

      ]

-------------------------------------------------------------------

type ListProps =
  ( query           :: SearchQuery
  , corpusId        :: CorpusId
  , listId          :: ListId
  , frontends       :: Frontends
  , session         :: Session
  )

contactList :: R2.Leaf ListProps
contactList = R2.leaf contactListCpt

contactListCpt :: R.Component ListProps
contactListCpt = here.component "main" cpt where
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
      , corpusId: nodeId
      , listId
      } _ = do
    -- | States
    -- |

    path' /\ path
      <- R2.useBox' $ initialPagePath { nodeId, listId, query, session }

    state' /\ state <-
      R2.useBox' Nothing

    rows' /\ rows <-
      R2.useBox' Nothing

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
        Contacts { contacts } -> T.write_ (Just contacts) rows
        _                     -> T.write_ (Just Seq.empty) rows

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
              H.text "No contact found in your corpus for your selected terms"
            ]
        ,
          R2.when (not $ eq results Seq.empty) $

            H.ul
            { className: intercalate " "
                [ "graph-contact-list"
                , "list-group"
                ]
            } $
            Seq.toUnfoldable $ flip Seq.map results \r ->

              item
              { frontends
              , session
              , contactView: (r :: ContactsView)
              }
        ]


---------------------------------------------------------

type ItemProps =
  ( contactView  :: ContactsView
  , frontends    :: Frontends
  , session      :: Session
  )

item :: R2.Leaf ItemProps
item = R2.leaf itemCpt

itemCpt :: R.Component ItemProps
itemCpt = here.component "item" cpt where
  cpt { contactView: ContactsView
          { id
          , annuaireId
          , hyperdata: HyperdataRowContact
              { firstname
              , lastname
              , labs
              }
          }
      , frontends
      , session
      } _ = do
    -- Computed
    let

      -- Creating a href link
      contactUrl id'
        = url frontends $ Routes.ContactPage (sessionId session) annuaireId id'

    -- Render
    pure $

      H.div
      { className: intercalate " "
          [ "graph-contact-list__item"
          , "list-group-item"
          ]
      }
      [
        H.a
        { className: "graph-contact-list__item__title"
        , target: "_blank"
        , href: contactUrl id
        }
        [
          H.text $ firstname
        ,
          H.text $ nbsp 1
        ,
          H.text $ lastname
        ]
      ,
        B.div'
        { className: "graph-contact-list__item__subtitle" }
        labs
      ]
