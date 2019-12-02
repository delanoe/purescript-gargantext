module Gargantext.Components.GraphExplorer.Sidebar
  (Props, sidebar)
  where

import Prelude
import Data.Array (head)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple.Nested((/\))
import Reactix as R
import Reactix.DOM.HTML as RH

import Gargantext.Data.Array (catMaybes)
import Gargantext.Components.RandomText (words)
import Gargantext.Components.Nodes.Corpus.Graph.Tabs as GT
import Gargantext.Components.Graph as Graph
import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Ends (Frontends)
import Gargantext.Hooks.Sigmax.Types as SigmaxTypes
import Gargantext.Sessions (Session)

type Props =
  ( frontends :: Frontends
  , graph :: Graph.Graph
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
      let nodesMap = SigmaxTypes.nodesMap props.graph

      pure $
        RH.div { id: "sp-container", className: "col-md-2" }
        [ RH.div {}
          [ RH.div { className: "row" }
            [ RH.div { className: "col-md-12" }
              [ RH.ul { id: "myTab", className: "nav nav-tabs", role: "tablist"}
                [ RH.li { className: "nav-item" }
                  [ RH.a { id: "home-tab"
                         , className: "nav-link active"
                         , data: {toggle: "tab"}
                         , href: "#home"
                         , role: "tab"
                         , aria: {controls: "home", selected: "true"}}
                    [ RH.text "Selected nodes" ] ] ]
              , RH.div { className: "tab-content", id: "myTabContent" }
                [ RH.div { className: "", id: "home", role: "tabpanel" }
                  (badge <$> badges props.selectedNodeIds nodesMap) ] ]
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
    badge text =
      RH.a { className: "badge badge-light" } [ RH.text text ]
    checkbox text =
      RH.li {}
      [ RH.span {} [ RH.text text ]
      , RH.input { type: "checkbox"
                 , className: "checkbox"
                 , checked: true
                 , title: "Mark as completed" } ]
    badges (selectedNodeIds /\ _) nodesMap = map (\n -> n.label)
                                           $ catMaybes
                                           $ map (\n -> Map.lookup n nodesMap)
                                           $ Set.toUnfoldable selectedNodeIds

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


