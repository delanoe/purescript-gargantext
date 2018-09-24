module Gargantext.Pages.Corpus.User.Users.Specs.Documents where

import Prelude
import React.DOM (table, tbody, td, text, th, thead, tr)
import React.DOM.Props (className, scope)
import Thermite (Render, Spec, defaultPerformAction, simpleSpec)

publicationSpec :: Spec {} {} Void
publicationSpec = simpleSpec defaultPerformAction render
  where
    render :: Render {} {} Void
    render dispatch _ state _ =
      [ table [ className "table"]
                  [ thead [ className "thead-dark"]
                    [ tr []
                      [ th [ scope "col"] [ text "Date"        ]
                      , th [ scope "col"] [ text "Description" ]
                      , th [ scope "col"] [ text "Projects"    ]
                      , th [ scope "col"] [ text "Favorite"    ]
                      , th [ scope "col"] [ text "Delete"      ]
                      ]
                    ]
                  , tbody []

                    [ tr [] [ td [] [ text "2012/03/06"]
                            , td [] [ text "Big data and text mining"]
                            , td [] [ text "European funds"]
                            , td [] [ text "True"]
                            , td [] [ text "False"]
                            ]
                    , tr [] [ td [] [ text "2013/03/06"]
                            , td [] [ text "Cryptography"]
                            , td [] [ text "French funds"]
                            , td [] [ text "True"]
                            , td [] [ text "False"]
                            ]
                    , tr [] [ td [] [ text "2013/03/06"]
                            , td [] [ text "Artificial Intelligence"]
                            , td [] [ text "Not found"]
                            , td [] [ text "True"]
                            , td [] [ text "False"]
                            ]
                    ]
                  ]
           ]
