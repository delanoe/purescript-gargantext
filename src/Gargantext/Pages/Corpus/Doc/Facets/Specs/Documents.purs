module Gargantext.Pages.Corpus.Doc.Facets.Documents where

import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)
import Prelude (id, void)
import React.DOM (table, tbody, td, text, th, thead, tr)
import React.DOM.Props (className, scope)
import Thermite (PerformAction, Render, Spec, modifyState, simpleSpec)


type State = String

initialState :: State
initialState = ""

data Action = NoOp

performAction :: forall eff props. PerformAction ( console :: CONSOLE
                                                 , ajax    :: AJAX
                                                 , dom     :: DOM
                                                 | eff
                                                 ) State props Action
performAction NoOp _ _ = void do
  modifyState id



publicationSpec :: forall props eff . Spec ( console :: CONSOLE
                                        , ajax    :: AJAX
                                        , dom     :: DOM
                                        | eff
                                        ) State props Action
publicationSpec = simpleSpec performAction render
  where
    render :: Render State props Action
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
