module Gargantext.Pages.Layout.Specs.Search where

import Prelude hiding (div)
import React.DOM (br', button, div, h3, input, text, i, span, img)
import React.DOM.Props (_id, _type, className, name, onClick, onInput, placeholder, value, aria, src, title)
import Thermite (PerformAction, Render, Spec, modifyState, simpleSpec)
import Unsafe.Coerce (unsafeCoerce)
import Gargantext.Pages.Home as L

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
  = NoOp
  | GO
  | SetQuery String


performAction :: forall props. PerformAction State props Action
performAction NoOp _ _ = void do
  modifyState id


performAction (SetQuery q) _ _ = void do
   modifyState \( state) ->  state { query = q }


performAction GO _ _ = void do
  lift $ setHash "/addCorpus"
  modifyState id


unsafeEventValue :: forall event. event -> String
unsafeEventValue e = (unsafeCoerce e).target.value

searchSpec :: forall props. Spec State props Action
searchSpec = simpleSpec performAction render
  where
    render :: Render State props Action
    render dispatch _ state _ =
      [ div [className "container1"] []
      , div [className "container1"]
       [ div [className "jumbotron" ]
         [ div [className "row"       ]
           [ div [className "col-md-10" ]
             [ br' []
             , br' []
             , div [ className "form-group"]
                   [ input [ className "form-control"
                           , _id "id_password"
                           , name "query"
                           , placeholder "Query, URL or FILE (works best with Firefox or Chromium browsers)"
                           , _type "text"
                           , value state.query
                           , onInput \e -> dispatch (SetQuery (unsafeEventValue e))
                           ] []
                    , br'[]
                    ]
              ]
            , div [ className "col-md-2"]
                  [ br' []
                  , br' []
                  , button [onClick \_ -> dispatch GO] [text "GO"]
                  ]
            , br' []
            ]
          ]
        ]
      ]
