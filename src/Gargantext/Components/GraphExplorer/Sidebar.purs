module Gargantext.Components.GraphExplorer.Sidebar
       where

import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Prelude
import Reactix as R
import Reactix.DOM.HTML as RH

import Gargantext.Components.GraphExplorer.Controls as Controls
import Gargantext.Utils.Reactix as R2


sidebar :: Record Controls.Controls -> R.Element
sidebar controls = R.createElement sidebarCpt controls []

sidebarCpt :: R.Component Controls.Controls
sidebarCpt = R.hooksComponent "Sidebar" cpt
  where
    cpt {showSidePanel: (false /\ _)} _children = do
      pure $ RH.div {} []
    cpt props _children = do
      pure $
        RH.div { id: "sp-container"
               , className: "col-md-2" }
        [
          RH.div {id: "sp-container"}
          [
            RH.div { className: "row" }
            [
              RH.div { className: "col-md-12" }
              [
                RH.ul { id: "myTab"
                      , className: "nav nav-tabs"
                      , role: "tablist"}
                [
                  RH.li { className: "nav-item" }
                  [
                    RH.a { id: "home-tab"
                         , className: "nav-link active"
                         , data: {toggle: "tab"}
                         , href: "#home"
                         , role: "tab"
                         , aria: {controls: "home", selected: "true"}}
                    [
                      RH.text "Neighbours"
                    ]
                  ]
                ]
              , RH.div { className: "tab-content"
                       , id: "myTabContent" }
                [ RH.div { className: ""
                         , id: "home"
                         , role: "tabpanel" }
                  [ badge "objects"
                  , badge "evaluation"
                  , badge "dynamics"
                  , badge "virtual environments"
                  , badge "virtual reality"
                  , badge "performance analysis"
                  , badge "software engineering"
                  , badge "complex systems"
                  , badge "wireless communications"
                  ]
                ]
              ]
            ]
          ]
        ]

    badge text =
      RH.a { className: "badge badge-light" } [ RH.text text ]
