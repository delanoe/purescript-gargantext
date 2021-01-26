module Gargantext.Components.GraphExplorer.Sidebar
  -- (Props, sidebar)
  where

import Control.Parallel (parTraverse)
import Data.Array (head, last, concat)
import Data.Int (fromString)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Sequence as Seq
import Data.Set as Set
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Reactix as R
import Reactix.DOM.HTML as RH
import Reactix.DOM.HTML as H

import Gargantext.Prelude

import Gargantext.Components.Lang (Lang(..))
import Gargantext.Components.Search (SearchType(..), SearchQuery(..))
import Gargantext.Components.GraphExplorer.Types  as GET
import Gargantext.Components.GraphExplorer.Types  (SidePanelState(..), SideTab(..))
import Gargantext.Components.GraphExplorer.Legend as Legend
import Gargantext.Components.NgramsTable.Core as NTC
import Gargantext.Components.Nodes.Corpus.Graph.Tabs (tabs) as CGT
import Gargantext.Components.RandomText (words)
import Gargantext.Data.Array (mapMaybe)
import Gargantext.Ends (Frontends)
import Gargantext.Hooks.Sigmax.Types as SigmaxT
import Gargantext.Sessions (Session)
import Gargantext.Types (CTabNgramType, TabSubType(..), TabType(..), TermList(..), modeTabType)
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Reload as GUR
import Partial.Unsafe (unsafePartial)

thisModule = "Gargantext.Components.GraphExplorer.Sidebar"

type Props =
  ( frontends       :: Frontends
  , graph           :: SigmaxT.SGraph
  , graphId         :: Int
  , graphVersion    :: GUR.ReloadS
  , metaData        :: GET.MetaData
  , removedNodeIds  :: R.State SigmaxT.NodeIds
  , selectedNodeIds :: R.State SigmaxT.NodeIds
  , session         :: Session
  , showSidePanel   :: R.State GET.SidePanelState
  , treeReload      :: GUR.ReloadS
  )

sidebar :: Record Props -> R.Element
sidebar props = R.createElement sidebarCpt props []
  where
    sidebarCpt :: R.Component Props
    sidebarCpt = R.hooksComponentWithModule thisModule "sidebar" cpt

    cpt {showSidePanel: (GET.Closed /\ _)} _children = do
      pure $ RH.div {} []
    cpt {showSidePanel: (GET.InitialClosed /\ _)} _children = do
      pure $ RH.div {} []
    cpt props@{metaData, showSidePanel} _children = do
      pure $ RH.div { id: "sp-container" }
        [ sideTabNav showSidePanel [SideTabLegend, SideTabData, SideTabCommunity]
        , sideTab (fst showSidePanel) props
        ]

sideTabNav :: R.State SidePanelState -> Array SideTab -> R.Element
sideTabNav (sidePanel /\ setSidePanel) sideTabs =
  R.fragment [ H.div { className: "text-primary center"} [H.text ""]
                     , H.div {className: "nav nav-tabs"} (liItem <$> sideTabs)
                     -- , H.div {className: "center"} [ H.text "Doc sideTabs"]
             ]
    where
      liItem :: SideTab -> R.Element
      liItem  tab =
        H.div { className : "nav-item nav-link"
                          <> if (Opened tab) == sidePanel
                               then " active"
                               else ""
            , on: { click: \_ -> setSidePanel $ const (Opened tab)
                  }
            } [ H.text $ show tab ]

sideTab :: SidePanelState -> Record Props -> R.Element
sideTab (Opened SideTabLegend) props@{metaData} =
  H.div {} [ let (GET.MetaData {legend}) = metaData
                    in Legend.legend { items: Seq.fromFoldable legend}
           , documentation EN
           ]

sideTab (Opened SideTabData) props =
  RH.div {} [ selectedNodes props (SigmaxT.nodesGraphMap props.graph)
            , neighborhood  props
            , RH.div { className: "col-md-12", id: "query" }
                     [ query SearchDoc
                             props.frontends
                             props.metaData
                             props.session
                             (SigmaxT.nodesGraphMap props.graph)
                             props.selectedNodeIds
                     ]
            ]
    where

      checkbox text =
        RH.li {}
        [ RH.span {} [ RH.text text ]
        , RH.input { type: "checkbox"
                   , className: "checkbox"
                   , defaultChecked: true
                   , title: "Mark as completed" } ]


sideTab (Opened SideTabCommunity) props  =
  RH.div { className: "col-md-12", id: "query" }
                         [ selectedNodes props (SigmaxT.nodesGraphMap props.graph)
                         , neighborhood  props
                         , query SearchContact
                                 props.frontends
                                 props.metaData
                                 props.session
                                 (SigmaxT.nodesGraphMap props.graph)
                                 props.selectedNodeIds
                         ]

sideTab _ _  = H.div {} []


-------------------------------------------
-- TODO
-- selectedNodes :: Record Props -> Map.Map String Nodes -> R.Element
selectedNodes props nodesMap = R2.row [ R2.col 12
                  [ RH.ul { id: "myTab", className: "nav nav-tabs", role: "tablist"}
                    [ RH.div { className: "tab-content" }
                      [ RH.div { className: "", role: "tabpanel" }
                               ( Seq.toUnfoldable
                               $ ( Seq.map (badge              props.selectedNodeIds)
                                           (badges props.graph props.selectedNodeIds)
                                 )
                               )
                      ]
                    , RH.div { className: "tab-content flex-space-between" }
                             [ removeButton "Move as candidate" CandidateTerm props nodesMap
                             , removeButton "Move as stop"      StopTerm      props nodesMap
                             ]
                    ]
                   ]
               ]
neighborhood props = RH.div { className: "tab-content", id: "myTabContent" }
                            [ RH.div { className: "", id: "home", role: "tabpanel" }
                              (Seq.toUnfoldable $ Seq.map (badge props.selectedNodeIds)
                                                $ neighbourBadges props.graph props.selectedNodeIds
                               )
                            ]


removeButton text rType props' nodesMap' =
  if Set.isEmpty $ fst props'.selectedNodeIds then
    RH.div {} []
  else
    RH.button { className: "btn btn-info"
              , on: { click: onClickRemove rType props' nodesMap' }
              }
              [ RH.text text ]

onClickRemove rType props' nodesMap' e = do
  let nodes = mapMaybe (\id -> Map.lookup id nodesMap')
                       $ Set.toUnfoldable $ fst props'.selectedNodeIds
  deleteNodes { graphId: props'.graphId
              , metaData: props'.metaData
              , nodes
              , session: props'.session
              , termList: rType
              , treeReload: props'.treeReload }
  snd props'.removedNodeIds  $ const $ fst props'.selectedNodeIds
  snd props'.selectedNodeIds $ const SigmaxT.emptyNodeIds



badge :: R.State SigmaxT.NodeIds -> Record SigmaxT.Node -> R.Element
badge (_ /\ setNodeIds) {id, label} =
  RH.a { className: "badge badge-light"
       , on: { click: onClick }
       } [ RH.text label ]
  where
    onClick e = do
      setNodeIds $ const $ Set.singleton id

badges :: SigmaxT.SGraph -> R.State SigmaxT.NodeIds -> Seq.Seq (Record SigmaxT.Node)
badges graph (selectedNodeIds /\ _) = SigmaxT.graphNodes $ SigmaxT.nodesById graph selectedNodeIds

neighbourBadges :: SigmaxT.SGraph -> R.State SigmaxT.NodeIds -> Seq.Seq (Record SigmaxT.Node)
neighbourBadges graph (selectedNodeIds /\ _) = SigmaxT.neighbours graph selectedNodes
  where
    selectedNodes = SigmaxT.graphNodes $ SigmaxT.nodesById graph selectedNodeIds


type DeleteNodes =
  ( graphId    :: Int
  , metaData   :: GET.MetaData
  , nodes      :: Array (Record SigmaxT.Node)
  , session    :: Session
  , termList   :: TermList
  , treeReload :: GUR.ReloadS
  )

deleteNodes :: Record DeleteNodes -> Effect Unit
deleteNodes { graphId, metaData, nodes, session, termList, treeReload } = do
  launchAff_ do
    patches <- (parTraverse (deleteNode termList session metaData) nodes) :: Aff (Array NTC.VersionedNgramsPatches)
    let mPatch = last patches
    case mPatch of
      Nothing -> pure unit
      Just (NTC.Versioned patch) -> do
        liftEffect $ GUR.bump treeReload

-- Why is this called delete node?
deleteNode :: TermList
           -> Session
           -> GET.MetaData
           -> Record SigmaxT.Node
           -> Aff NTC.VersionedNgramsPatches
deleteNode termList session (GET.MetaData metaData) node = do
    ret  <- NTC.putNgramsPatches coreParams versioned
    task <- NTC.postNgramsChartsAsync coreParams  -- TODO add task
    pure ret
  where
    nodeId :: Int
    nodeId = unsafePartial $ fromJust $ fromString node.id

    versioned :: NTC.VersionedNgramsPatches
    versioned = NTC.Versioned {version: metaData.list.version, data: np}

    coreParams :: NTC.CoreParams ()
    coreParams = {session, nodeId, listIds: [metaData.list.listId], tabType}

    tabNgramType :: CTabNgramType
    tabNgramType = modeTabType node.gargType

    tabType :: TabType
    tabType = TabCorpus (TabNgramType tabNgramType)

    term :: NTC.NgramsTerm
    term = NTC.normNgram tabNgramType node.label

    pt :: NTC.NgramsTablePatch
    pt = NTC.fromNgramsPatches np

    np :: NTC.NgramsPatches
    np = NTC.singletonPatchMap term $ NTC.NgramsPatch { patch_children: mempty, patch_list }

    patch_list :: NTC.Replace TermList
    patch_list = NTC.Replace { new: termList, old: MapTerm }

query :: SearchType
      -> Frontends
      -> GET.MetaData
      -> Session
      -> SigmaxT.NodesMap
      -> R.State SigmaxT.NodeIds
      -> R.Element
query _ _ _ _ _ (selectedNodeIds /\ _) | Set.isEmpty selectedNodeIds = RH.div {} []
query searchType frontends (GET.MetaData metaData) session nodesMap (selectedNodeIds /\ _) =
  query' (head metaData.corpusId)
  where
    query' Nothing         = RH.div {} []
    query' (Just corpusId) =
      CGT.tabs { frontends
               , session
               , query: SearchQuery { query : concat $ toQuery <$> Set.toUnfoldable selectedNodeIds
                                    , expected: searchType
                                    }
               , sides: [side corpusId]
               }

    toQuery id = case Map.lookup id nodesMap of
      Nothing -> []
      Just n -> words n.label

    side corpusId = GET.GraphSideCorpus { corpusId
                                        , listId     : metaData.list.listId
                                        , corpusLabel: metaData.title
                                        }

------------------------------------------------------------------------

            {-, RH.div { className: "col-md-12", id: "horizontal-checkbox" }
              [ RH.ul {}
                [ checkbox "Pubs"
                , checkbox "Projects"
                , checkbox "Patents"
                , checkbox "Others"
                ]
              ]
              -}
--------------------------------------------------------------------------


documentation :: Lang -> R.Element
documentation _ =
  H.div {} [ H.h2 {} [ H.text "What is Graph ?"]
           , ul [ "Graph is a conveniant tool to explore your documents. "
                , "Nodes are terms selected in your Map List. "
                <> "Node size is proportional to the number of documents with the associated term. "
                , "Edges between nodes represent proximities of terms according to a specific distance between your documents. "
                <> "Link strength is proportional to the strenght of terms association."
                ]
           , H.h3 {} [ H.text "Basic Interactions:"]
           , ul [ "Click on a node to select/unselect and get its information. "
                , "In case of multiple selection, the button unselect clears all selections. "
                <> "Use your mouse scroll to zoom in and out in the graph. "
                , "Use the node filter to create a subgraph with nodes of a given size "
                <>"range (e.g. display only generic terms). "
                , "Use the edge filter so create a subgraph with links in a given range (e.g. keep the strongest association)."
                ]
           ]

  where
    ul ts = H.ul {} $ map (\t -> H.li {} [ H.text t ]) ts

{-
TODO DOC
  Conditional distance between the terms X and Y is the probability to have both terms X and Y in the same textual context.
  Distributional distance between the terms X and Y is the probability to have same others terms in the same textual context as X or Y.

Global/local view:
    The 'change level' button allows to change between global view and node centered view,
    To explore the neighborhood of a selection click on the 'change level' button.
-}

