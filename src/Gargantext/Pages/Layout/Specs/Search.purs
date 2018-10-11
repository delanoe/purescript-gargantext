module Gargantext.Pages.Layout.Specs.Search where

import Prelude hiding (div)

import Effect.Class (liftEffect)
import React.DOM (br', button, div, input, text)
import React.DOM.Props (_id, _type, className, name, onClick, onInput, placeholder, value)
import Routing.Hash (setHash)
import Thermite (PerformAction, Render, Spec, modifyState, simpleSpec)
import Unsafe.Coerce (unsafeCoerce)

type State =
  {
    query :: String
  }


initialState :: State
initialState =
  {
    query : "empty query"
  }


data Action
  = GO
  | SetQuery String


unsafeEventValue :: forall event. event -> String
unsafeEventValue e = (unsafeCoerce e).target.value

searchSpec :: Spec State {} Action
searchSpec = simpleSpec performAction render
  where
    performAction :: PerformAction State {} Action
    performAction (SetQuery q) _ _ = void do
      modifyState $ _ { query = q }

    performAction GO _ _ = void do
      liftEffect $ setHash "/addCorpus"

    render :: Render State {} Action
    render dispatch _ state _ =
      [ div [className "container1"] []
      , div [className "container1"]
       [ div [className "jumbotron" ]
         [ div [className "row"       ]
           [ div [className "col-md-10" ]
             [ br'
             , br'
             , div [ className "form-group"]
                   [ input [ className "form-control"
                           , _id "id_password"
                           , name "query"
                           , placeholder "Query, URL or FILE (works best with Firefox or Chromium browsers)"
                           , _type "text"
                           , value state.query
                           , onInput \e -> dispatch (SetQuery (unsafeEventValue e))
                           ]
                    , br'
                    ]
              ]
            , div [ className "col-md-2"]
                  [ br'
                  , br'
                  , button [onClick \_ -> dispatch GO] [text "GO"]
                  ]
            , br'
            ]
          ]
        ]
      ]
