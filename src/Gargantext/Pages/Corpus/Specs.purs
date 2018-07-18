module Gargantext.Pages.Corpus.Specs where

import Prelude hiding (div)

import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (log)
import Control.Monad.Cont.Trans (lift)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Lens (over)
import Data.Maybe (Maybe(Just))
import Data.MediaType.Common (applicationJSON)

import Gargantext.Components.Modals.Modal (modalHide)

import Gargantext.Pages.Corpus.States
import Gargantext.Pages.Corpus.Actions

import Network.HTTP.Affjax (AJAX, affjax, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(..))
import React (ReactElement)
import React.DOM (button, div, h3, h5, li, span, text, ul)
import React.DOM.Props (_data, _id, _type, aria, className, onClick, role)
import Routing.Hash.Aff (setHash)
import Thermite (PerformAction, Render, Spec, _render, cotransform, modifyState, simpleSpec)



modalSpec :: forall eff props. Boolean -> String -> Spec eff State props Action -> Spec eff State props Action
modalSpec sm t = over _render \render d p s c ->
  [ div [ _id "addCorpus", className $ "modal myModal" <> if sm then "" else " fade"
            , role "dialog"
            , _data {show : true}
            ][ div [ className "modal-dialog"
                   , role "document"
                   ] [ div [ className "modal-content"]
                       [ div [ className "modal-header"]
                         [ h5 [ className "modal-title"
                              ]
                           [ text $ t
                           ]
                         , button [ _type "button"
                                  , className "close"
                                  , _data { dismiss : "modal"}
                                  ] [ span [ aria {hidden : true}]
                                      [ text "X"]
                                    ]
                         ]

                       , div [ className "modal-body"]
                         (render d p s c)
                       ]
                     ]
             ]
  ]

spec' :: forall eff props. Spec (console:: CONSOLE, ajax :: AJAX, dom :: DOM | eff) State props Action
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
                                        [ h5 [ className "modal-title"
                                             ]
                                             [ text "CorpusView"
                                             ]
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



layoutAddcorpus :: forall props eff . Spec (console::CONSOLE, ajax::AJAX, dom::DOM | eff) State props Action
layoutAddcorpus = simpleSpec performAction render
  where
    render :: Render State props Action
    render dispatch _ state _ =
      [
        div [className "container1"] []
      ,  div [className "container1"]
        [
          div [className "jumbotron"]
          [ div [className "row"]
           [
             div [className "col-md-6"] (layoutModal state)
           , div [className "col-md-6"]
             [
               h3 [] [text "Corpusview"]
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



