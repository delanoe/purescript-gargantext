module Gargantext.Components.GraphExplorer.Sidebar
  (Props, sidebar)
  where

import Prelude

import Control.Parallel (parTraverse)
import Data.Array (last, uncons)
import Data.Int (fromString)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Sequence as Seq
import Data.Set as Set
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Partial.Unsafe (unsafePartial)
import Reactix as R
import Reactix.DOM.HTML as RH

import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Components.NgramsTable.Core as NTC
import Gargantext.Components.Nodes.Corpus.Graph.Tabs as GT
import Gargantext.Components.RandomText (words)
import Gargantext.Data.Array (mapMaybe)
import Gargantext.Ends (Frontends)
import Gargantext.Hooks.Sigmax.Types as SigmaxT
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, post)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2

type Props =
  ( frontends :: Frontends
  , graph :: SigmaxT.SGraph
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
sidebarCpt = R.hooksComponent "G.C.GE.S.sidebar" cpt
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
                , RH.div { className: "tab-content" }
                  [
                    removeButton "Remove candidate" GT.CandidateTerm props nodesMap
                  , removeButton "Remove stop" GT.StopTerm props nodesMap
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
                query {
                  frontends: props.frontends
                , metaData: props.metaData
                , nodesMap
                , selectedNodeIds: props.selectedNodeIds
                , session: props.session
                }
              ]
            ]
          ]
        ]
    checkbox text =
      RH.li {}
      [ RH.span {} [ RH.text text ]
      , RH.input { type: "checkbox"
                 , className: "checkbox"
                 , defaultChecked: true
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
      deleteNodes rType props.session props.metaData props.graphVersion nodes
      snd props.removedNodeIds $ const $ fst props.selectedNodeIds
      snd props.selectedNodeIds $ const SigmaxT.emptyNodeIds


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

deleteNodes :: GT.TermList -> Session -> GET.MetaData -> R.State Int -> Array (Record SigmaxT.Node) -> Effect Unit
deleteNodes termList session metaData (_ /\ setGraphVersion) nodes = do
  launchAff_ do
    patches <- (parTraverse (deleteNode termList session metaData) nodes) :: Aff (Array NTC.VersionedNgramsPatches)
    let mPatch = last patches
    case mPatch of
      Nothing -> pure unit
      Just (NTC.Versioned patch) -> pure unit --liftEffect do
        --setGraphVersion $ const $ patch.version

deleteNode :: GT.TermList -> Session -> GET.MetaData -> Record SigmaxT.Node -> Aff NTC.VersionedNgramsPatches
deleteNode termList session (GET.MetaData metaData) node = NTC.putNgramsPatches coreParams versioned
  where
    nodeId :: Int
    nodeId = unsafePartial $ fromJust $ fromString node.id
    versioned :: NTC.VersionedNgramsPatches
    versioned = NTC.Versioned {version: metaData.list.version, data: np}
    coreParams :: NTC.CoreParams ()
    coreParams = {session, nodeId: nodeId, listIds: [metaData.list.listId], tabType}
    tabNgramType :: GT.CTabNgramType
    tabNgramType = GT.modeTabType node.gargType
    tabType :: GT.TabType
    tabType = GT.TabCorpus (GT.TabNgramType tabNgramType)
    term :: NTC.NgramsTerm
    term = NTC.normNgram tabNgramType node.label
    pt :: NTC.NgramsTablePatch
    pt = NTC.fromNgramsPatches np
    np :: NTC.NgramsPatches
    np = NTC.singletonPatchMap term $ NTC.NgramsPatch { patch_children: mempty, patch_list }
    patch_list :: NTC.Replace GT.TermList
    patch_list = NTC.Replace { new: termList, old: GT.GraphTerm }

type QueryProps =
  (
    frontends :: Frontends
  , metaData :: GET.MetaData
  , nodesMap :: SigmaxT.NodesMap
  , selectedNodeIds :: R.State SigmaxT.NodeIds
  , session :: Session
  )

query :: Record QueryProps -> R.Element
query props = R.createElement queryCpt props []

queryCpt :: R.Component QueryProps
queryCpt = R.hooksComponent "G.C.GE.S.query" cpt
  where
    cpt {selectedNodeIds: (selectedNodeIds /\ _)} _ | Set.isEmpty selectedNodeIds = pure $ RH.div {} []
    cpt {frontends, metaData: (GET.MetaData metaData@{corpusId}), nodesMap, selectedNodeIds: (selectedNodeIds /\ _), session} _ = case uncons corpusId of
      Nothing -> pure $ RH.div {} []
      Just {head: corpusId} ->
        pure $ RH.div {} [
            pairing corpusId
          , query' corpusId
        ]
      where
        query' cId =
          GT.tabs { frontends
                  , query: q <$> Set.toUnfoldable selectedNodeIds
                  , session
                  , sides: [side cId]}
        q id = case Map.lookup id nodesMap of
          Nothing -> []
          Just n -> words n.label
        side cId = GET.GraphSideCorpus {
              corpusId: cId
            , listId: metaData.list.listId
            , corpusLabel: metaData.title
            }
        pairing cId =
          RH.div {} [ RH.span { className: "btn btn-default"
                              , on: { click: onClickPair }
                              } [ RH.text "Pair with Annuaire" ] ]
          where
            onClickPair _ = do
              let labels = Map.values $ Map.mapMaybe (\n -> if Set.member n.id selectedNodeIds then Just n.label else Nothing) nodesMap
              launchAff_ $ do
                pairWithAnnuaire { corpusId: cId
                                 , listId: metaData.list.listId
                                 , query: List.toUnfoldable labels
                                 , session }

pairWithAnnuaire :: { corpusId :: Int
                    , listId :: Int
                    , query :: Array String
                    , session :: Session } -> Aff Unit
pairWithAnnuaire {corpusId, listId, query, session} =
  post session (NodeAPI GT.Node (Just corpusId) $ "searchPair/list/" <> (show listId)) {query}
