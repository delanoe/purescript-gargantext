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
import Gargantext.Sessions (Session)
import Gargantext.Types (CTabNgramType, TabSubType(..), TabType(..), TermList(..), modeTabType)
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

type MRemovalRoot = Maybe SigmaxT.NodeId

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
      selectedNodeIdsRef <- R.useRef $ fst props.selectedNodeIds
      mRemovalRoot <- R.useState' $ getMRemovalRoot props.selectedNodeIds

      -- we need to handle changing of selectedNodeIds, otherwise mRemovalRoot
      -- won't be automatically updated
      R.useEffect' do
        if R.readRef selectedNodeIdsRef == fst props.selectedNodeIds then
          pure unit
        else do
          R.setRef selectedNodeIdsRef $ fst props.selectedNodeIds
          snd mRemovalRoot $ const $ getMRemovalRoot props.selectedNodeIds

      pure $
        RH.div { id: "sp-container" }
        [ RH.div {}
          [ R2.row
            [ R2.col12
              [ RH.ul { id: "myTab", className: "nav nav-tabs", role: "tablist"}
                [ RH.div { className: "tab-content" }
                  [ RH.div { className: "", role: "tabpanel" }
                    $ badges props.graph props.selectedNodeIds mRemovalRoot
                  ]
                , RH.div { className: "tab-content" }
                  [
                    removeButton "Remove candidate" CandidateTerm props nodesMap mRemovalRoot
                  , removeButton "Remove stop" StopTerm props nodesMap mRemovalRoot
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
                  $ neighbourBadges props.graph props.selectedNodeIds
                ]
              ]
            , RH.div { className: "col-md-12", id: "query" }
              [
                query props.frontends props.metaData props.session nodesMap props.selectedNodeIds
              ]
            ]
          ]
        ]

    getMRemovalRoot :: R.State SigmaxT.NodeIds -> MRemovalRoot
    getMRemovalRoot (selectedNodeIds /\ _) = head $ Set.toUnfoldable selectedNodeIds


removeButton :: String -> TermList -> Record Props -> SigmaxT.NodesMap -> MRemovalRoot -> R.Element
removeButton text rType props nodesMap Nothing = RH.div {} []
removeButton text rType props nodesMap (Just removalRoot) = RH.div {} []
  RH.button { className: "btn btn-danger"
            , on: { click: onClickRemove }}
  [ RH.text text ]
  where
    onClickRemove :: forall e. e -> Effect Unit
    onClickRemove e = do
      let nodes = mapMaybe (\id -> Map.lookup id nodesMap) $ Set.toUnfoldable $ fst props.selectedNodeIds
      case Map.lookup removalRoot nodesMap of
        Nothing -> pure unit
        Just removalRootNode -> do
          deleteNodes rType props.session props.metaData props.graphVersion removalRootNode $ filter (/= removalRootNode) nodes
          snd props.removedNodeIds $ const $ fst props.selectedNodeIds
          snd props.selectedNodeIds $ const SigmaxT.emptyNodeIds


badges :: SigmaxT.SGraph -> R.State SigmaxT.NodeIds -> R.State (Maybe SigmaxT.NodeId) -> Array R.Element
badges graph selectedNodeIds mRemovalRoot = Seq.toUnfoldable $ Seq.map (badgeRR selectedNodeIds mRemovalRoot) (badgeNodes graph selectedNodeIds)

badge :: R.State SigmaxT.NodeIds -> String -> Array R.Element -> Record SigmaxT.Node -> R.Element
badge (_ /\ setNodeIds) additionalClassName children {id, label} =
  RH.a { className: "badge badge-light " <> additionalClassName }
    $ [
      RH.span { on: { click: onClick } } [ RH.text label ]
    ] <> children
  where
    onClick _ = do
      setNodeIds $ const $ Set.singleton id

badgeRR :: R.State SigmaxT.NodeIds -> R.State (Maybe SigmaxT.NodeId) -> Record SigmaxT.Node -> R.Element
badgeRR selectedNodeIds (mRemovalRoot /\ setMRemovalRoot) node@{id} = badge selectedNodeIds additionalClassName children node
  where
    children = [ RH.span { className, on: { click: onClick } } [] ]
    className = if mRemovalRoot == (Just id) then "" else "glyphicon glyphicon-chevron-up"
    additionalClassName = if mRemovalRoot == (Just id) then "badge-primary" else ""
    onClick _ = do
      setMRemovalRoot $ const $ Just id

badgeNodes :: SigmaxT.SGraph -> R.State SigmaxT.NodeIds -> Seq.Seq (Record SigmaxT.Node)
badgeNodes graph (selectedNodeIds /\ _) = SigmaxT.graphNodes $ SigmaxT.nodesById graph selectedNodeIds

neighbourBadges :: SigmaxT.SGraph -> R.State SigmaxT.NodeIds -> Array R.Element
neighbourBadges graph selectedNodeIds =  Seq.toUnfoldable $ Seq.map (badge selectedNodeIds "" []) (neighbourBadgeNodes graph selectedNodeIds)

neighbourBadgeNodes :: SigmaxT.SGraph -> R.State SigmaxT.NodeIds -> Seq.Seq (Record SigmaxT.Node)
neighbourBadgeNodes graph (selectedNodeIds /\ _) = SigmaxT.neighbours graph selectedNodes
  where
    selectedNodes = SigmaxT.graphNodes $ SigmaxT.nodesById graph selectedNodeIds


-- API
deleteNodes :: TermList -> Session -> GET.MetaData -> R.State Int -> Record SigmaxT.Node -> Array (Record SigmaxT.Node) -> Effect Unit
deleteNodes termList session metaData (_ /\ setGraphVersion) rootNode childNodes = do
  launchAff_ do
    patch <- deleteRootNode termList session metaData rootNode childNodes
    --patches <- (parTraverse (deleteNode termList session metaData) nodes) :: Aff (Array NTC.VersionedNgramsPatches)
    --let mPatch = last patches
    --case mPatch of
    --  Nothing -> pure unit
    --  Just (NTC.Versioned patch) -> pure unit
    --    --liftEffect do
    --    --setGraphVersion $ const $ patch.version

deleteRootNode :: TermList -> Session -> GET.MetaData -> Record SigmaxT.Node -> Array (Record SigmaxT.Node) -> Aff NTC.VersionedNgramsPatches
deleteRootNode termList session (GET.MetaData metaData) rootNode childNodes = NTC.putNgramsPatches coreParams versioned
  where
    nodeId = unsafePartial <<< fromJust <<< fromString <<< _.id
    rootNodeId :: Int
    rootNodeId = nodeId rootNode
    childNodeIds :: Array Int
    childNodeIds = nodeId <$> childNodes
    versioned :: NTC.VersionedNgramsPatches
    versioned = NTC.Versioned {version: metaData.list.version, data: np}
    coreParams :: NTC.CoreParams ()
    coreParams = {session, nodeId: rootNodeId, listIds: [metaData.list.listId], tabType}
    tabNgramType :: CTabNgramType
    tabNgramType = modeTabType rootNode.gargType
    tabType :: TabType
    tabType = TabCorpus (TabNgramType tabNgramType)
    term :: NTC.NgramsTerm
    term = NTC.normNgram tabNgramType node.label
    pt :: NTC.NgramsTablePatch
    pt = NTC.fromNgramsPatches np

patch :: TermList -> TermList -> Record SigmaxT.Node -> Array (Record SigmaxT.Node) -> NTC.NgramsPatches
patch termList oldTermList node childNodes = np
  where
    np :: NTC.NgramsPatches
    np = NTC.singletonPatchMap term $ NTC.NgramsPatch { patch_children, patch_list }
    patch_list :: NTC.Replace TermList
    patch_list = NTC.Replace { new: termList, old: oldTermList }
    patch_children :: NTC.PatchSet NTC.NgramsTerm
    patch_children = NTC.patchSetFromMap $ Map.fromFoldable $

deleteNode :: TermList -> Session -> GET.MetaData -> Record SigmaxT.Node -> Aff NTC.VersionedNgramsPatches
deleteNode termList session (GET.MetaData metaData) node = NTC.putNgramsPatches coreParams versioned
  where
    nodeId :: Int
    nodeId = unsafePartial $ fromJust $ fromString node.id
    versioned :: NTC.VersionedNgramsPatches
    versioned = NTC.Versioned {version: metaData.list.version, data: np}
    coreParams :: NTC.CoreParams ()
    coreParams = {session, nodeId: nodeId, listIds: [metaData.list.listId], tabType}
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
      GT.tabs {frontends, session, query: q <$> Set.toUnfoldable selectedNodeIds, sides: [side corpusId]}
    q id = case Map.lookup id nodesMap of
      Nothing -> []
      Just n -> words n.label
    side corpusId = GET.GraphSideCorpus {
          corpusId
        , listId: metaData.list.listId
        , corpusLabel: metaData.title
        }
