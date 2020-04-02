module Gargantext.Components.GraphExplorer.Sidebar
  (Props, sidebar)
  where

import Prelude

import Control.Parallel (parTraverse)
import Data.Array (head, last)
import Data.Int (fromString)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Sequence as Seq
import Data.Set as Set
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log2)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Timer (setInterval)
import Partial.Unsafe (unsafePartial)
import Reactix as R
import Reactix.DOM.HTML as RH

import Gargantext.Components.GraphExplorer.API as GAPI
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Components.NgramsTable.Core as NTC
import Gargantext.Components.Nodes.Corpus.Graph.Tabs (tabs) as CGT
import Gargantext.Components.RandomText (words)
import Gargantext.Data.Array (mapMaybe)
import Gargantext.Ends (Frontends, url)
import Gargantext.Hooks.Sigmax.Types as SigmaxT
import Gargantext.Routes as Routes
import Gargantext.Sessions (Session)
import Gargantext.Types (CTabNgramType, TabSubType(..), TabType(..), TermList(..), modeTabType)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2

type Props =
  ( frontends :: Frontends
  , graph :: SigmaxT.SGraph
  , graphId :: Int
  , graphVersion :: R.State Int
  , metaData :: GET.MetaData
  , removedNodeIds :: R.State SigmaxT.NodeIds
  , selectedNodeIds :: R.State SigmaxT.NodeIds
  , session :: Session
  , showSidePanel :: GET.SidePanelState
  )

sidebar :: Record Props -> R.Element
sidebar props = R.createElement sidebarCpt props []

sidebarCpt :: R.Component Props
sidebarCpt = R.hooksComponent "Sidebar" cpt
  where
    cpt {showSidePanel: GET.Closed} _children = do
      pure $ RH.div {} []
    cpt {showSidePanel: GET.InitialClosed} _children = do
      pure $ RH.div {} []
    cpt props _children = do
      let nodesMap = SigmaxT.nodesGraphMap props.graph

      pure $
        RH.div { id: "sp-container" }
        [ RH.div {}
          [ R2.row
            [ R2.col12
              [ RH.ul { id: "myTab", className: "nav nav-tabs", role: "tablist"}
                [ RH.div { className: "tab-content" }
                  [ RH.div { className: "", role: "tabpanel" }
                    (Seq.toUnfoldable $ (Seq.map (badge props.selectedNodeIds) (badges props.graph props.selectedNodeIds)))
                  ]
                , RH.div { className: "gexf" } [
                   RH.a { className: "btn btn-default"
                        , href: gexfHref props.session props.graphId
                        , target: "_blank" } [ RH.text "Download GEXF" ]
                  ]
                , RH.div { className: "tab-content" }
                  [
                    removeButton "Remove candidate" CandidateTerm props nodesMap
                  , removeButton "Remove stop" StopTerm props nodesMap
                  ]
                , RH.li { className: "nav-item" }
                  [ RH.a { id: "home-tab"
                         , className: "nav-link active"
                         , data: {toggle: "tab"}
                         , href: "#home"
                         , role: "tab"
                         , aria: {controls: "home", selected: "true"}
                         }
                    [ RH.text "Neighbours" ]
                  ]
                ]
              , RH.div { className: "tab-content", id: "myTabContent" }
                [ RH.div { className: "", id: "home", role: "tabpanel" }
                  (Seq.toUnfoldable $ (Seq.map (badge props.selectedNodeIds) (neighbourBadges props.graph props.selectedNodeIds)))
                ]
              ]
            {-, RH.div { className: "col-md-12", id: "horizontal-checkbox" }
              [ RH.ul {}
                [ checkbox "Pubs"
                , checkbox "Projects"
                , checkbox "Patents"
                , checkbox "Others"
                ]
              ]
              -}
            , RH.div { className: "col-md-12", id: "query" }
              [
                query props.frontends props.metaData props.session nodesMap props.selectedNodeIds
              ]
            ]
          ]
        ]
    checkbox text =
      RH.li {}
      [ RH.span {} [ RH.text text ]
      , RH.input { type: "checkbox"
                 , className: "checkbox"
                 , checked: true
                 , title: "Mark as completed" } ]

    removeButton text rType props nodesMap =
      if Set.isEmpty $ fst props.selectedNodeIds then
        RH.div {} []
      else
        RH.button { className: "btn btn-danger"
                  , on: { click: onClickRemove rType props nodesMap }}
        [ RH.text text ]

    onClickRemove rType props nodesMap e = do
      let nodes = mapMaybe (\id -> Map.lookup id nodesMap) $ Set.toUnfoldable $ fst props.selectedNodeIds
      deleteNodes rType props.session props.metaData props.graphId nodes
      snd props.removedNodeIds $ const $ fst props.selectedNodeIds
      snd props.selectedNodeIds $ const SigmaxT.emptyNodeIds

    gexfHref :: Session -> Int -> String
    gexfHref session graphId = url session $ Routes.NodeAPI GT.Graph (Just graphId) "gexf"


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

deleteNodes :: TermList -> Session -> GET.MetaData -> Int -> Array (Record SigmaxT.Node) -> Effect Unit
deleteNodes termList session metaData graphId nodes = do
  -- launchAff_ do
  --   task <- GAPI.graphAsyncUpdate { graphId, listId, nodes, termList, session, version }
  --   liftEffect $ log2 "task" task
  -- where
  --   listId = metaData.list.listId
  --   version = metaData.list.version

  launchAff_ do
    patches <- (parTraverse (deleteNode termList session metaData) nodes) :: Aff (Array NTC.VersionedNgramsPatches)
    let mPatch = last patches
    case mPatch of
      Nothing -> pure unit
      Just (NTC.Versioned patch) -> do
        task <- GAPI.graphAsyncRecompute { graphId, session }
        _ <- liftEffect $ setInterval 1000 $ launchAff_ $ do
          let (GT.AsyncTaskWithType { task: GT.AsyncTask { id } }) = task
          asyncProgress@(GT.AsyncProgress {status}) <- GAPI.queryProgress { graphId, session, taskId: id }
          liftEffect $ log2 "progress" asyncProgress
        pure unit
        --pure unit
        --liftEffect do
        --setGraphVersion $ const $ patch.version

deleteNode :: TermList -> Session -> GET.MetaData -> Record SigmaxT.Node -> Aff NTC.VersionedNgramsPatches
deleteNode termList session (GET.MetaData metaData) node = NTC.putNgramsPatches coreParams versioned
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
    patch_list = NTC.Replace { new: termList, old: GraphTerm }


query :: Frontends -> GET.MetaData -> Session -> SigmaxT.NodesMap -> R.State SigmaxT.NodeIds -> R.Element
query _ _ _ _ (selectedNodeIds /\ _) | Set.isEmpty selectedNodeIds = RH.div {} []
query frontends (GET.MetaData metaData) session nodesMap (selectedNodeIds /\ _) =
  query' (head metaData.corpusId)
  where
    query' Nothing = RH.div {} []
    query' (Just corpusId) =
      CGT.tabs {frontends, session, query: q <$> Set.toUnfoldable selectedNodeIds, sides: [side corpusId]}
    q id = case Map.lookup id nodesMap of
      Nothing -> []
      Just n -> words n.label
    side corpusId = GET.GraphSideCorpus {
          corpusId
        , listId: metaData.list.listId
        , corpusLabel: metaData.title
        }
