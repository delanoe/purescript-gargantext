module Gargantext.Components.GraphExplorer.Sidebar
  (Props, sidebar)
  where

import Prelude
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple.Nested((/\))
import Reactix as R
import Reactix.DOM.HTML as RH

import Gargantext.Components.RandomText (words)
import Gargantext.Components.Nodes.Corpus.Graph.Tabs as GT
import Gargantext.Components.Graph as Graph
import Gargantext.Hooks.Sigmax.Types as SigmaxTypes
import Gargantext.Sessions (Session)

type Props =
  ( graph :: Graph.Graph
  , selectedNodeIds :: R.State SigmaxTypes.SelectedNodeIds
  , session :: Session
  , showSidePanel :: Boolean
  )

sidebar :: Record Props -> R.Element
sidebar props = R.createElement sidebarCpt props []

sidebarCpt :: R.Component Props
sidebarCpt = R.hooksComponent "Sidebar" cpt
  where
    cpt {showSidePanel: false} _children = do
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
                    [ RH.text "Neighbours" ] ] ]
              , RH.div { className: "tab-content", id: "myTabContent" }
                [ RH.div { className: "", id: "home", role: "tabpanel" }
                  (badge <$> badges) ] ]
            , RH.div { className: "col-md-12", id: "horizontal-checkbox" }
              [ RH.ul {}
                [ checkbox "Pubs"
                , checkbox "Projects"
                , checkbox "Patents"
                , checkbox "Others"
                ]
              ]
            , RH.div { className: "col-md-12", id: "query" }
              [
                query props.session nodesMap props.selectedNodeIds
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
    badges =
      [ "objects"
      , "evaluation"
      , "dynamics"
      , "virtual environments"
      , "virtual reality"
      , "performance analysis"
      , "software engineering"
      , "complex systems"
      , "wireless communications" ]

    query session nodesMap (selectedNodeIds /\ _) =
      GT.tabs {session, query: q <$> Set.toUnfoldable selectedNodeIds, sides: []}
      where
        q id = case Map.lookup id nodesMap of
          Nothing -> []
          Just n -> words n.label
