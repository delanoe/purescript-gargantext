module Gargantext.Components.GraphExplorer.Sidebar
  ( sidebar
  ) where

import Gargantext.Prelude

import Control.Parallel (parTraverse)
import Data.Array (last)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Foldable as F
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (fromString)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Number as DN
import Data.Sequence as Seq
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Gargantext.Components.App.Store as AppStore
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), Variant(..))
import Gargantext.Components.GraphExplorer.Sidebar.ContactList (contactListWrapper)
import Gargantext.Components.GraphExplorer.Sidebar.DocList (docListWrapper)
import Gargantext.Components.GraphExplorer.Sidebar.Legend as Legend
import Gargantext.Components.GraphExplorer.Store as GraphStore
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Components.GraphExplorer.Utils as GEU
import Gargantext.Components.Lang (Lang(..))
import Gargantext.Config.REST (AffRESTError)
import Gargantext.Core.NgramsTable.Functions as NTC
import Gargantext.Core.NgramsTable.Types as CNT
import Gargantext.Data.Array (mapMaybe)
import Gargantext.Ends (Frontends)
import Gargantext.Hooks.FirstEffect (useFirstEffect')
import Gargantext.Hooks.Sigmax.Types as SigmaxT
import Gargantext.Sessions (Session)
import Gargantext.Types (CTabNgramType, FrontendError(..), NodeID, TabSubType(..), TabType(..), TermList(..), modeTabType)
import Gargantext.Utils (getter, nbsp, setter, (?))
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Partial.Unsafe (unsafePartial)
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer.Sidebar"

type Props =
  ( metaData        :: GET.MetaData
  , session         :: Session
  , frontends       :: Frontends
  )

sidebar :: R2.Leaf Props
sidebar = R2.leaf sidebarCpt

sidebarCpt :: R.Component Props
sidebarCpt = here.component "sidebar" cpt where
  cpt props _ = do
    -- States
    { sideTab
    } <- GraphStore.use

    sideTab'  <- R2.useLive' sideTab

    -- Computed
    let
      sideTabs =
        [ GET.SideTabLegend
        , GET.SideTabData
        , GET.SideTabCommunity
        ]

    -- Render
    pure $

      H.div
      { className: "graph-sidebar" }
      [
        -- Menu
        B.tabs
        { value: sideTab'
        , list: sideTabs
        , callback: flip T.write_ sideTab
        }
      ,
        case sideTab' of
          GET.SideTabLegend     -> sideTabLegend props
          GET.SideTabData       -> sideTabData props
          GET.SideTabCommunity  -> sideTabCommunity props
      ]

------------------------------------------------------------

sideTabLegend :: R2.Leaf Props
sideTabLegend = R2.leaf sideTabLegendCpt

sideTabLegendCpt :: R.Component Props
sideTabLegendCpt = here.component "sideTabLegend" cpt where
  cpt { metaData: GET.MetaData { legend } } _ = do
    -- | States
    -- |
    store <- GraphStore.use

    hyperdataGraph
      <- R2.useLive' store.hyperdataGraph

    -- | Computed
    -- |
    let
      maxItemPerCluster = 4

    -- | Hooks
    -- |

    -- For each provided Cluster (see Legend), extract the greatest nodes
    extractedNodeList <- R.useMemo1 hyperdataGraph $ const $
      flip A.foldMap legend
      (   getter _.id_
      >>> GEU.takeGreatestNodeByCluster
            hyperdataGraph
            maxItemPerCluster
      )

    -- For each provided Cluster (see Legend), count the number of nodes
    nodeCountList <- R.useMemo1 hyperdataGraph $ const $
      flip A.foldMap legend
      (   getter _.id_
      >>> GEU.countNodeByCluster hyperdataGraph
      >>> A.singleton
      )

    -- | Render
    -- |
    pure $

      H.div
      { className: "graph-sidebar__legend-tab" }
      [
        Legend.legend
        { legendSeq: Seq.fromFoldable legend
        , extractedNodeList
        , nodeCountList
        , selectedNodeIds: store.selectedNodeIds
        }
      ,
        H.hr {}
      ,
        documentation EN
      ]

------------------------------------------------------------

sideTabData :: R2.Leaf Props
sideTabData = R2.leaf sideTabDataCpt
sideTabDataCpt :: R.Component Props
sideTabDataCpt = here.component "sideTabData" cpt where
  cpt props _ = do
    -- States
    { selectedNodeIds
    , graph
    } <- GraphStore.use

    selectedNodeIds'  <- R2.useLive' selectedNodeIds
    graph'            <- R2.useLive' graph

    -- Computed
    let
      hasSelection = not $ Set.isEmpty selectedNodeIds'

    -- Render
    pure $

      H.div
      { className: "graph-sidebar__data-tab" }
      [
        case hasSelection of

          -- No result
          false ->

            B.caveat
            {}
            [
              H.text "Select one or more nodes to get their informations"
            ]

          -- Nodes have been selected
          true ->

            R.fragment
            [
              selectedNodes $
              { nodesMap: SigmaxT.nodesGraphMap graph'
              } `Record.merge` props
            ,
              sideBarTabSeparator
            ,
              neighborhood
              {}
            ,
              sideBarTabSeparator
            ,
              docListWrapper
              { metaData: props.metaData
              }
            ]
      ]

------------------------------------------------------------

sideTabCommunity :: R2.Leaf Props
sideTabCommunity = R2.leaf sideTabCommunityCpt

sideTabCommunityCpt :: R.Component Props
sideTabCommunityCpt = here.component "sideTabCommunity" cpt where
  cpt props _ = do
    -- States
    { selectedNodeIds
    , graph
    } <- GraphStore.use

    selectedNodeIds'  <- R2.useLive' selectedNodeIds
    graph'            <- R2.useLive' graph

    -- Computed
    let
      hasSelection = not $ Set.isEmpty selectedNodeIds'

    -- Render
    pure $

      H.div
      { className: "graph-sidebar__community-tab" }
      [
        case hasSelection of

          -- No result
          false ->

            B.caveat
            {}
            [
              H.text "Select one or more nodes to get their informations"
            ]

          -- Nodes have been selection
          true ->

            R.fragment
            [
              selectedNodes $
              { nodesMap: SigmaxT.nodesGraphMap graph'
              } `Record.merge` props
            ,
              sideBarTabSeparator
            ,
              neighborhood
              {}
            ,
              sideBarTabSeparator
            ,
              contactListWrapper
              { metaData: props.metaData
              }
            ]
      ]

-------------------------------------------

sideBarTabSeparator :: R.Element
sideBarTabSeparator =
  H.div
  { className: "graph-sidebar__separator" }
  [
    B.icon
    { name: "angle-double-down" }
  ]

-------------------------------------------
-- TODO
-- selectedNodes :: Record Props -> Map.Map String Nodes -> R.Element

type SelectedNodesProps =
  ( nodesMap :: SigmaxT.NodesMap
  | Props
  )

selectedNodes :: R2.Leaf SelectedNodesProps
selectedNodes = R2.leaf selectedNodesCpt

selectedNodesCpt :: R.Component SelectedNodesProps
selectedNodesCpt = here.component "selectedNodes" cpt where
  cpt props _ = do
    -- | States
    -- |
    { selectedNodeIds
    , graph
    , expandSelection
    } <- GraphStore.use

    selectedNodeIds'    <- R2.useLive' selectedNodeIds
    graph'              <- R2.useLive' graph
    expandSelection'    <- R2.useLive' expandSelection

    -- | Effects
    -- |

    -- transfer local Component change to Local Storage cache
    useFirstEffect' $
      flip T.listen expandSelection onExpandSelectionChange

    -- | Behaviors
    -- |
    let
      onBadgeClick id _ = T.write_ (Set.singleton id) selectedNodeIds

      onExpandClick _ = T.modify_ (not) expandSelection

    -- | Render
    -- |
    pure $

      H.ul
      { className: intercalate " "
          [ "graph-selected-nodes"
          , "list-group"
          ]
        }
      [
        H.li
        { className: "list-group-item" }
        [
          H.ul
          {} $

          Seq.toUnfoldable $
            flip Seq.map (badges graph' selectedNodeIds') \node ->

              H.li
              { className: "graph-selected-nodes__item" }
              [
                H.a
                { className: intercalate " "
                    [ "graph-selected-nodes__badge"
                    , "badge badge-info"
                    ]
                , on: { click: onBadgeClick node.id }
                }
                [ H.text node.label ]
              ]
        ,
          -- Expand NGrams actions
          B.iconButton
          { name: expandSelection' ?
              "caret-up" $
              "caret-down"
          , className: "graph-selected-nodes__expand"
          , callback: onExpandClick
          }
        ]
      ,
        -- NGrams actions
        R2.when expandSelection' $

          H.li
          { className: intercalate " "
              [ "list-group-item"
              , "graph-selected-nodes__actions"
              ]
          }
          [
            B.buttonGroup
            { collapse: false }
            [
              updateTermButton
              ( props `Record.merge`
                { variant: ButtonVariant Light
                , rType: CandidateTerm
                }
              )
              [
                B.icon
                { name: "circle"
                , className: "mr-1 candidate-term"
                }
              ,
                H.text "Move as candidate"
              ]
            ,
              updateTermButton
              ( props `Record.merge`
                { variant: ButtonVariant Light
                , rType: StopTerm
                }
              )
              [
                B.icon
                { name: "circle"
                , className: "mr-1 stop-term"
                }
              ,
                H.text "Move as stop"
              ]
            ]
          ]
      ]

onExpandSelectionChange :: T.Change Boolean -> Effect Unit
onExpandSelectionChange { new } = do
  cache <- R2.loadLocalStorageState' R2.graphParamsKey GET.defaultCacheParams
  let update = setter (_ { expandSelection = new }) cache
  R2.setLocalStorageState R2.graphParamsKey update

---------------------------------------------------------

neighborhood :: R2.Leaf ()
neighborhood = R2.leaf neighborhoodCpt
neighborhoodCpt :: R.Memo ()
neighborhoodCpt = R.memo' $ here.component "neighborhood" cpt where
  cpt _ _ = do
    -- | States
    -- |
    { selectedNodeIds
    , graph
    , expandNeighborhood
    } <- GraphStore.use

    selectedNodeIds' <-
      R2.useLive' selectedNodeIds

    expandNeighborhood' <-
      R2.useLive' expandNeighborhood

    graph' <-
      R2.useLive' graph

    showMore /\ showMoreBox <-
      R2.useBox' false

    termList /\ termListBox <-
      R2.useBox' []

    termCount /\ termCountBox <-
      R2.useBox' 0

    -- | Computed
    -- |
    let
      minSize = F.foldl DN.min 0.0 (Seq.map _.size (SigmaxT.graphNodes graph'))

      maxSize = F.foldl DN.max 0.0 (Seq.map _.size (SigmaxT.graphNodes graph'))

      maxTruncateResult = 5

      withTruncateResults = (termCount > maxTruncateResult) && (not showMore)


    -- | Behaviors
    -- |
    let
      onBadgeClick id _ = T.write_ (Set.singleton id) selectedNodeIds

      onExpandClick _ = T.modify_ (not) expandNeighborhood

    -- | Effects
    -- |

    -- transfer local Component change to Local Storage cache
    useFirstEffect' $
      flip T.listen expandNeighborhood onExpandNeighborhoodChange

    R.useEffect1' selectedNodeIds' do
      let refreshed = neighbourBadges graph' selectedNodeIds'
      let count     = Seq.length refreshed
      let ordered   = A.sortWith (\n -> -n.size) $ Seq.toUnfoldable refreshed
      T.write_ count   termCountBox
      T.write_ ordered termListBox
      T.write_ false showMoreBox

    -- | Render
    -- |
    pure $

      H.ul
      { className: intercalate " "
          [ "graph-neighborhood"
          , "list-group"
          ]
      }
      [
        -- Extracted count
        H.li
        { className: "list-group-item" }
        [
          -- @XXX: Bootstrap CSS w/ one <li> deduped the list-style-type bullet
          H.div
          { className: "graph-neighborhood__counter" }
          [
            B.wad'
            [ "text-info", "d-inline" ] $
            show termCount
          ,
            H.text $ nbsp 1 <> "related terms"
          ,
            -- Expand word cloud
            B.iconButton
            { name: expandNeighborhood' ?
                "caret-up" $
                "caret-down"
            , className: "graph-neighborhood__expand"
            , callback: onExpandClick
            }
          ]
        ]
      ,
        -- Word cloud
        R2.when expandNeighborhood' $

          H.li
          { className: "list-group-item"}
          [
            H.ul
            {} $
            flip mapWithIndex termList \index node ->

              R2.when
              (
                (withTruncateResults == false
                || index < maxTruncateResult)
                && (not $ Set.member node.id selectedNodeIds')
              ) $
                H.li
                { className: "graph-neighborhood__badge" }
                [
                  H.a
                  { className: "badge badge-light"
                  -- adjust font accordingly
                  , style:
                      { fontSize: badgeSize
                          minSize
                          maxSize
                          node.size
                      , lineHeight: badgeSize
                          minSize
                          maxSize
                          node.size
                      }
                  , on: { click: onBadgeClick node.id }
                  }
                  [ H.text node.label ]
                ]
          ,
            R2.when (withTruncateResults) $

              B.button
              { variant: ButtonVariant Light
              , callback: \_ -> T.modify_ (not) showMoreBox
              , block: true
              , className: "graph-neighborhood__show-more"
              }
              [
                H.text "Show more"
              ]
          ]
      ]

onExpandNeighborhoodChange :: T.Change Boolean -> Effect Unit
onExpandNeighborhoodChange { new } = do
  cache <- R2.loadLocalStorageState' R2.graphParamsKey GET.defaultCacheParams
  let update = setter (_ { expandNeighborhood = new }) cache
  R2.setLocalStorageState R2.graphParamsKey update

---------------------------------------------------------

type UpdateTermButtonProps =
  ( variant    :: ButtonVariant
  , nodesMap   :: SigmaxT.NodesMap
  , rType      :: TermList
  | Props
  )

updateTermButton :: R2.Component UpdateTermButtonProps
updateTermButton = R2.component updateTermButtonCpt

updateTermButtonCpt :: R.Component UpdateTermButtonProps
updateTermButtonCpt = here.component "updateTermButton" cpt where
  cpt { variant
      , metaData
      , nodesMap
      , rType
      , session
      } children = do
    -- States
    { errors
    , reloadForest
    } <- AppStore.use

    { removedNodeIds
    , selectedNodeIds
    , graphId
    } <- GraphStore.use

    selectedNodeIds' <- R2.useLive' selectedNodeIds
    graphId'         <- R2.useLive' graphId

    -- Behaviors
    let
      callback _ = do
        let nodes = mapMaybe (\id -> Map.lookup id nodesMap)
                            $ Set.toUnfoldable selectedNodeIds'
        sendPatches { errors
                    , graphId: graphId'
                    , metaData: metaData
                    , nodes
                    , session: session
                    , termList: rType
                    , reloadForest
                    }
        T.write_ selectedNodeIds' removedNodeIds
        T.write_ SigmaxT.emptyNodeIds selectedNodeIds

    -- Render
    pure $

      B.button
      { variant
      , callback
      }
      children


---------------------------------------------------------

badgeSize :: Number -> Number -> Number -> String
badgeSize minSize maxSize size =
  let
    minFontSize = 7.0
    maxFontSize = 28.0
    sizeScaled = (size - minSize) / (maxSize - minSize)  -- in [0; 1] range
    --scale' = DN.log (sizeScaled + 1.0) / (DN.log 2.0)  -- in [0; 1] range
    --scale = minFontSize + scale' * (maxFontSize - minFontSize)
    scale = minFontSize + sizeScaled * (maxFontSize - minFontSize)

  in
    show scale <> "px"


badges :: SigmaxT.SGraph -> SigmaxT.NodeIds -> Seq.Seq (Record SigmaxT.Node)
badges graph selectedNodeIds = SigmaxT.graphNodes $ SigmaxT.nodesById graph selectedNodeIds

neighbourBadges :: SigmaxT.SGraph -> SigmaxT.NodeIds -> Seq.Seq (Record SigmaxT.Node)
neighbourBadges graph selectedNodeIds = SigmaxT.neighbours graph selectedNodes' where
  selectedNodes' = SigmaxT.graphNodes $ SigmaxT.nodesById graph selectedNodeIds

---------------------------------------------------------

type SendPatches =
  ( errors       :: T.Box (Array FrontendError)
  , graphId      :: NodeID
  , metaData     :: GET.MetaData
  , nodes        :: Array (Record SigmaxT.Node)
  , reloadForest :: T2.ReloadS
  , session      :: Session
  , termList     :: TermList
  )

sendPatches :: Record SendPatches -> Effect Unit
sendPatches { errors, metaData, nodes, reloadForest, session, termList } = do
  launchAff_ do
    patches <- (parTraverse (sendPatch termList session metaData) nodes) -- :: Aff (Array CNT.VersionedNgramsPatches)
    let mPatch = last patches
    case mPatch of
      Nothing -> pure unit
      Just (Left err) -> liftEffect $ do
        T.modify_ (A.cons $ FRESTError { error: err }) errors
        here.warn2 "[sendPatches] RESTError" err
      Just (Right (CNT.Versioned _patch)) -> do
        liftEffect $ T2.reload reloadForest

-- Why is this called delete node?
sendPatch :: TermList
          -> Session
          -> GET.MetaData
          -> Record SigmaxT.Node
          -> AffRESTError CNT.VersionedNgramsPatches
sendPatch termList session (GET.MetaData metaData) node = do
    eRet  <- NTC.putNgramsPatches coreParams versioned
    case eRet of
      Left err -> pure $ Left err
      Right ret -> do
        _task <- NTC.postNgramsChartsAsync coreParams  -- TODO add task
        pure $ Right ret
  where
    nodeId :: NodeID
    nodeId = unsafePartial $ fromJust $ fromString node.id

    versioned :: CNT.VersionedNgramsPatches
    versioned = CNT.Versioned {version: metaData.list.version, data: np}

    coreParams :: CNT.CoreParams ()
    coreParams = {session, nodeId, listIds: [metaData.list.listId], tabType}

    tabNgramType :: CTabNgramType
    tabNgramType = modeTabType node.gargType

    tabType :: TabType
    tabType = TabCorpus (TabNgramType tabNgramType)

    term :: CNT.NgramsTerm
    term = NTC.normNgram tabNgramType node.label

    np :: CNT.NgramsPatches
    np = NTC.singletonPatchMap term $ CNT.NgramsPatch { patch_children: mempty, patch_list }

    patch_list :: CNT.Replace TermList
    patch_list = CNT.Replace { new: termList, old: MapTerm }



-----------------------------------------------------
documentation :: Lang -> R.Element
documentation _ =

    H.div
    { className: "graph-documentation" }
    [
      H.div
      { className: "graph-documentation__text-section" }
      [
        H.p
        {}
        [
          B.b_ "What is a graph? "
        ,
          H.text "Graph is a conveniant tool to explore your documents."
        ]
      ,
        H.p
        {}
        [
          H.text $

            "Nodes are terms selected in your Map List. "
          <>
            "Node size is proportional to the number of documents with the associated term. "
        ]
      ,
        H.p
        {}
        [
          H.text $

            "Edges between nodes represent proximities of terms according to a specific distance between your documents. "
          <>
            "Link strength is proportional to the strenght of terms association."
        ]
      ]
    ,
      H.div
      { className: "graph-documentation__text-section" }
      [
        H.ul
        {}
        [
          H.li
          {}
          [
            H.text $

              "Click on a node to select/unselect and get its information."
          ]
        ,
          H.li
          {}
          [
            H.text $

              "In case of multiple selection, the button unselect clears all selections. "
            <>
              "Use your mouse scroll to zoom in and out in the graph. "
          ]
        ,
          H.li
          {}
          [
            H.text $

              "Use the node filter to create a subgraph with nodes of a given size "
            <>
              "range (e.g. display only generic terms). "
          ]
        ,
          H.li
          {}
          [
            H.text $

              "Use the edge filter so create a subgraph with links in a given range (e.g. keep the strongest association)."
          ]
        ]
      ]
    ]

{-
TODO DOC
  Conditional distance between the terms X and Y is the probability to have both terms X and Y in the same textual context.
  Distributional distance between the terms X and Y is the probability to have same others terms in the same textual context as X or Y.

Global/local view:
    The 'change level' button allows to change between global view and node centered view,
    To explore the neighborhood of a selection click on the 'change level' button.
-}
