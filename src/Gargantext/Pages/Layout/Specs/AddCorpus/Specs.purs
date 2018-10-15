module Gargantext.Pages.Layout.Specs.AddCorpus.Specs where

import Data.Lens (over)
import Effect.Aff (Aff)
import React (ReactElement)
import React.DOM (button, div, h3, h5, li, span, text, ul)
import React.DOM.Props (_data, _id, _type, aria, className, onClick, role)
import Thermite (Render, Spec, _render, simpleSpec)

import Gargantext.Prelude
import Gargantext.Config.REST (post)
import Gargantext.Pages.Layout.Specs.AddCorpus.Actions (Action(..), performAction)
import Gargantext.Pages.Layout.Specs.AddCorpus.States (Query, Response(..), State)

modalSpec :: Boolean -> String -> Spec State {} Action -> Spec State {} Action
modalSpec sm t = over _render \render d p s c ->
  [ div [ _id "addCorpus", className $ "modal myModal" <> if sm then "" else " fade"
        , role "dialog"
        , _data {show : true}
        ][ div [ className "modal-dialog", role "document"]
               [ div [ className "modal-content"]
                     [ div [ className "modal-header"]
                           [ h5 [ className "modal-title" ] [ text $ t ]
                           , button [ _type "button"
                                    , className "close"
                                    , _data { dismiss : "modal"}
                                    ] [ span [ aria {hidden : true}] [ text "X"] ]
                           ]

                      , div [ className "modal-body"] (render d p s c)
                      ]
                ]
         ]
   ]


spec' :: Spec State {} Action
spec' = modalSpec true "Search Results" layoutAddcorpus


layoutModal :: forall e.  { response :: Array Response | e} -> Array ReactElement
layoutModal state =
      [button [ _type "button"
             , _data { "toggle" : "modal"
             , "target" : ".myModal"
             }
             ][text "Launch modal"]
             , div [ className "modal fade myModal"
                   , role "dialog"
                   , _data {show : true}
                   ][ div [ className "modal-dialog"
                          , role "document"
                          ] [ div [ className "modal-content"]
                                  [ div [ className "modal-header"]
                                        [ h5 [className "modal-title"]
                                             [text "CorpusView"      ]
                                        , button [ _type "button"
                                                 , className "close"
                                                 , _data { dismiss : "modal"}
                                                 ] [ span [ aria {hidden : true}]
                                                          [ text "X"]
                                                   ]
                                        ]

                                  , div [ className "modal-body"]
                                        [ ul [ className "list-group"] ( map fn1 state.response ) ]

                                  , div [className "modal-footer"]
                                        [ button [ _type "button"
                                                 , className "btn btn-secondary"
                                                 , _data {dismiss : "modal"}
                                                 ] [ text "GO"]
                                        ]
                                   ]
                            ]
                     ]
        ]
      where
        fn1 (Response o) =
          li [className "list-group-item justify-content-between"]
          [
          span [] [text  o.name]
          ,  span [className "badge badge-default badge-pill"] [ text $ show o.count]
          ]


layoutAddcorpus :: Spec State {} Action
layoutAddcorpus = simpleSpec performAction render
  where
    render :: Render State {} Action
    render dispatch _ state _ =
      [ div [className "container1"] []
      , div [className "container1"]
        [ div [className "jumbotron"]
          [ div [className "row"]
           [ div [className "col-md-6"] (layoutModal state)
           , div [className "col-md-6"]
             [ h3 [] [text "Corpusview"]
             , ul [className "list-group"] $ map fn1 state.response
             , button [onClick \_ -> dispatch GO] [text "GO"]
             ]
           ]
          ]
        ]
      ]
      where
        fn1 (Response o) =
          li [className "list-group-item justify-content-between"]
          [
          span [] [text  o.name]
          ,  span [className "badge badge-default badge-pill"] [ text $ show o.count]
          ]



countResults :: Query -> Aff Int
countResults = post "http://localhost:8008/count"
