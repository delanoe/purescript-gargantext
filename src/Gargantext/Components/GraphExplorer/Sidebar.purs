module Gargantext.Components.GraphExplorer.Sidebar
  (Props, sidebar)
  where

import Prelude
import Data.Array (head)
import Data.Int (fromString)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Sequence as Seq
import Data.Set as Set
import Data.Traversable (traverse_)
import Data.Tuple.Nested((/\))
import DOM.Simple.Console (log2)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Reactix as R
import Reactix.DOM.HTML as RH

import Gargantext.Components.RandomText (words)
import Gargantext.Components.Nodes.Corpus.Graph.Tabs as GT
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Data.Array (mapMaybe)
import Gargantext.Ends (Frontends)
import Gargantext.Hooks.Sigmax.Types as SigmaxTypes
import Gargantext.Routes (SessionRoute(NodeAPI))
import Gargantext.Sessions (Session, delete)
import Gargantext.Types (NodeType(..))
import Gargantext.Utils.Reactix as R2

type Props =
  ( frontends :: Frontends
  , graph :: SigmaxTypes.SGraph
  , metaData :: GET.MetaData
  , selectedNodeIds :: R.State SigmaxTypes.SelectedNodeIds
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
      let nodesMap = SigmaxTypes.nodesGraphMap props.graph

      pure $
        RH.div { id: "sp-container", className: "col-md-3" }
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
                    RH.button { className: "btn btn-danger"
                              , on: { click: onClickRemove props.session props.selectedNodeIds }}
                    [ RH.text "Remove" ]
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
    badge (_ /\ setSelectedNodeIds) {id, label} =
      RH.a { className: "badge badge-light"
           , on: { click: onClick }
           } [ RH.text label ]
      where
        onClick e = do
          setSelectedNodeIds $ const $ Set.singleton id
    checkbox text =
      RH.li {}
      [ RH.span {} [ RH.text text ]
      , RH.input { type: "checkbox"
                 , className: "checkbox"
                 , checked: true
                 , title: "Mark as completed" } ]
    badges :: SigmaxTypes.SGraph -> R.State SigmaxTypes.SelectedNodeIds -> Seq.Seq (Record SigmaxTypes.Node)
    badges graph (selectedNodeIds /\ _) = SigmaxTypes.nodesById graph selectedNodeIds
    neighbourBadges :: SigmaxTypes.SGraph -> R.State SigmaxTypes.SelectedNodeIds -> Seq.Seq (Record SigmaxTypes.Node)
    neighbourBadges graph (selectedNodeIds /\ _) = SigmaxTypes.neighbours graph selectedNodes
      where
        selectedNodes = SigmaxTypes.nodesById graph selectedNodeIds

    onClickRemove session (selectedNodeIds /\ _) e = do
      log2 "[onClickRemove] selectedNodeIds" selectedNodeIds
      let nodeIds = mapMaybe fromString $ Set.toUnfoldable selectedNodeIds
      deleteNodes session nodeIds

    deleteNodes :: Session -> Array Int -> Effect Unit
    deleteNodes session nodeIds = do
      traverse_ (launchAff_ <<< deleteNode session) nodeIds

    deleteNode :: Session -> Int -> Aff Int
    deleteNode session nodeId = delete session $ NodeAPI Node (Just nodeId) ""

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
           , listId: metaData.listId
           , corpusLabel: metaData.title
           }


