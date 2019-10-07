module Gargantext.Components.GraphExplorer.Sidebar (Props, sidebar) where

import Prelude
import Reactix as R
import Reactix.DOM.HTML as RH

type Props
  = ( showSidePanel :: Boolean )

sidebar :: Record Props -> R.Element
sidebar props = R.createElement sidebarCpt props []

sidebarCpt :: R.Component Props
sidebarCpt = R.hooksComponent "Sidebar" cpt
  where
  cpt { showSidePanel: false } _children = do
    pure $ RH.div {} []

  cpt props _children = do
    pure
      $ RH.div { id: "sp-container", className: "col-md-2" }
          [ RH.div {}
              [ RH.div { className: "row" }
                  [ RH.div { className: "col-md-12" }
                      [ RH.ul { id: "myTab", className: "nav nav-tabs", role: "tablist" }
                          [ RH.li { className: "nav-item" }
                              [ RH.a
                                  { id: "home-tab"
                                  , className: "nav-link active"
                                  , data: { toggle: "tab" }
                                  , href: "#home"
                                  , role: "tab"
                                  , aria: { controls: "home", selected: "true" }
                                  }
                                  [ RH.text "Neighbours" ]
                              ]
                          ]
                      , RH.div { className: "tab-content", id: "myTabContent" }
                          [ RH.div { className: "", id: "home", role: "tabpanel" }
                              (badge <$> badges)
                          ]
                      ]
                  , RH.div { className: "col-md-12", id: "horizontal-checkbox" }
                      [ RH.ul {}
                          [ checkbox "Pubs"
                          , checkbox "Projects"
                          , checkbox "Patents"
                          , checkbox "Others"
                          ]
                      ]
                  ]
              ]
          ]

  badge text = RH.a { className: "badge badge-light" } [ RH.text text ]

  checkbox text =
    RH.li {}
      [ RH.span {} [ RH.text text ]
      , RH.input
          { type: "checkbox"
          , className: "checkbox"
          , checked: true
          , title: "Mark as completed"
          }
      ]

  badges =
    [ "objects"
    , "evaluation"
    , "dynamics"
    , "virtual environments"
    , "virtual reality"
    , "performance analysis"
    , "software engineering"
    , "complex systems"
    , "wireless communications"
    ]
