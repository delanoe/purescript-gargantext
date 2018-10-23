module Gargantext.Pages.Layout.Specs.AddCorpus.Specs where

import Gargantext.Pages.Layout.Specs.AddCorpus.Actions
import Gargantext.Pages.Layout.Specs.AddCorpus.States
import Prelude hiding (div)

import Affjax (defaultRequest, printResponseFormatError, request)
import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Cont.Trans (lift)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Lens (over)
import Data.Maybe (Maybe(Just))
import Data.MediaType.Common (applicationJSON)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Gargantext.Components.Modals.Modal (modalHide)
import React (ReactElement)
import React.DOM (button, div, h3, h5, li, span, text, ul)
import React.DOM.Props (_data, _id, _type, aria, className, onClick, role)
import Thermite (PerformAction, Render, Spec, _render, simpleSpec)


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



countResults ::  Query -> Aff (Either String (Int))
countResults query = do
  res <- request $ defaultRequest
         { url = "http://localhost:8008/count"
         , responseFormat = ResponseFormat.json
         , method = Left POST
         , headers = []
         , content = Just $ Json $ encodeJson query
         }
  case res.body of
    Left err -> do
      _ <- liftEffect $ log $ printResponseFormatError err
      pure $ Left $ printResponseFormatError err
    Right json -> do
      --_ <- liftEffect $ log $ show a.status
      --_ <- liftEffect $ log $ show a.headers
      --_ <- liftEffect $ log $ show a.body
      let obj = decodeJson json
      pure obj
