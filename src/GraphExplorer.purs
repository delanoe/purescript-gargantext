module GraphExplorer where

import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import DOM.File.FileReader (fileReader, readAsText, readyState, result)
import DOM.File.FileReader.ReadyState (ReadyState(..))
import DOM.File.Types (File, FileReader, fileToBlob)
import Prelude hiding (div)
import React (ReactClass, createElement)
import React.DOM (a, button, button', div, form', input, li, li', menu, p, span, text, ul, ul')
import React.DOM.Props (_data, _id, _type, aria, checked, className, disabled, href, name, onChange, onClick, placeholder, role, style, title, value)
import Thermite (PerformAction, Render, Spec, modifyState, simpleSpec)
import Unsafe.Coerce (unsafeCoerce)

foreign import data GraphData :: Type

foreign import initialFile :: File

foreign import getFile :: forall e. e -> File

foreign import parseJSON :: forall eff a. a -> Eff eff GraphData

foreign import graphExplorerComponent :: ReactClass {graph :: GraphData, mode :: String}

foreign import logger :: forall a eff. a -> Eff eff Unit

foreign import setupProgress :: forall e a. FileReader -> a -> Eff e Unit

newtype State = State {mode :: String, graph :: GraphData, fileReader :: FileReader, readyState :: ReadyState}

data Action
  = SetGraph
  | SetFile File (forall e. ReadyState -> Eff e Unit)
  | SetProgress ReadyState

initialState :: State
initialState = State {mode : "select", graph : unsafeCoerce "", fileReader : unsafeCoerce "", readyState : EMPTY}

startProcessing :: forall eff. File -> Eff (console :: CONSOLE, dom :: DOM | eff) FileReader
startProcessing f = do
  fr <- fileReader
  _ <- readAsText (fileToBlob f) fr
  pure fr

getData :: forall eff. FileReader -> Eff  (console :: CONSOLE, dom :: DOM | eff) GraphData
getData fr = do
  res <- result fr
  parseJSON res

-- processingStatus :: FileReader -> Boolean
-- processingStatus fr = readyState fr == DONE

  -- rs <- readyState fr
  -- logger $ show rs
  -- res <- result fr
  -- logger fr
  -- logger "res"
  -- logger res
  -- logger "/res"
  -- da <- parseJSON res
  -- logger "parsed Data"
  -- logger da
  -- logger "/parsed data"

spec :: forall eff props. Spec (console :: CONSOLE, dom :: DOM | eff) State props Action
spec = simpleSpec performAction render
  where
    render :: Render State props Action
    render d _ (State st) _ =
      [  div [className "row"] [
            div [className "col-md-12", style {marginTop : "21px", marginBottom : "21px"}]
            [ menu [_id "toolbar"]
              [ ul'
                [
                  li'
                  [ button [className "btn btn-success btn-sm"] [text "Change Type"]
                  ]
                , li'
                  [ button [className "btn btn-primary btn-sm"] [text "Change Level"]
                  ]


                 ,li [style {display : "inline-block"}]
                  [ form'
                    [ input [_type "file", name "file", onChange (\e -> d $ SetFile (getFile e) (unsafeCoerce $ d <<< SetProgress)), className "btn btn-primary"] []

                    -- , text $ show st.readyState
                    ]
                  ]
                , li' [ input [_type "button", className "btn btn-warning btn-sm",value "Run Demo", onClick \_ -> d SetGraph, disabled (st.readyState /= DONE)] []
                      ]

                , li'
                  [ form'
                    [ div [className "col-lg-2"]
                      [
                        div [className "input-group"]
                        [
                          span [className "input-group-btn"]
                          [
                            button [className "btn btn-primary", _type "button"]
                            [ span [className "glyphicon glyphicon-search"] []
                            ]
                          ]
                          ,input [_type "text", className "form-control", placeholder "select topics"] []
                        ]
                      ]

                    ]
                  ]
                , li [className "col-md-2"]
                  [ span [] [text "selector size"],input [_type "range", _id "myRange", value "90"] []
                  ]
                , li [className "col-md-2"]
                  [ span [] [text "label size"],input [_type "range", _id "myRange", value "90"] []
                  ]

                , li [className "col-md-2"]
                  [ span [] [text "Nodes"],input [_type "range", _id "myRange", value "90"] []
                  ]
                , li [className "col-md-2"]
                  [ span [] [text "Edges"],input [_type "range", _id "myRange", value "90"] []
                  ]
                , li'
                  [ button [className "btn btn-primary"] [text "Save"] -- TODO: Implement Save!
                  ]
                ]
              ]
            ]
           ]
         , div [className "row"]
           [ div [className "col-md-9"]
             [ div [style {border : "1px black solid", height: "90%"}]
               [ createElement graphExplorerComponent { graph : st.graph
                                                      , mode : st.mode
                                                      } []
               ]
             ]
           , div [className "col-md-3", style {border : "1px black solid", height: "90%", backgroundColor : "beige"}]
             [ div [className "row"]
               [ div [_id "sidepanel" , className "col-md-12", style {borderBottom : "1px solid black"}]
               [ p []
                 [ text "memory/status/important issue/ design/ knowledge mangaement/ design/ theory/ system design"
                 ]
               , button [className "btn btn-primary", style {marginBottom : "18px"}] [text "Remove"]
               ]
             , div [className "col-md-12"]
               [
                 ul [className "nav nav-tabs", _id "myTab", role "tablist", style {marginBottom : "18px", marginTop : "18px"}]
                 [
                   li [className "nav-item"]
                   [
                     a [className "nav-link active"
                       , _id "home-tab"
                       ,  _data {toggle : "tab"}
                       , href "#home"
                       , role "tab"
                       , aria {controls :"home" , selected : "true"}
                       ] [text "Neighbours"]
                   ]
                 ]
                 , div [className "tab-content", _id "myTabContent", style {borderBottom : "1px solid black", paddingBottom : "19px"}]
                   [ div [ className "", _id "home", role "tabpanel" ]
                     [ a [ className "badge badge-light"][text "objects"]
                     , a [ className "badge badge-light"][text "evaluation"]
                     , a [ className "badge badge-light"][text "dynamics"]
                     , a [ className "badge badge-light"][text "virtual environments"]
                     , a [ className "badge badge-light"][text "virtual reality"]
                     , a [ className "badge badge-light"][text "performance analysis"]
                     , a [ className "badge badge-light"][text "software engineering"]
                     , a [ className "badge badge-light"][text "complex systems"]
                     , a [ className "badge badge-light"][text "wireless communications"]
                     ]
                   ]
                 ]
             , div [className "col-md-12", _id "horizontal-checkbox"]
               [ ul [ style {display: "inline",float : "left" }]
                 [ li []
                   [ span [] [text "Pubs"]
                     ,input [ _type "checkbox"
                           , className "checkbox"
                           , checked $ true
                           , title "Mark as completed"
                             --  , onChange $ dispatch <<< ( const $ SetMap $ not (getter _._type state.term == MapTerm))
                           ] []

                   ]
                 , li []
                   [ span [] [text "Projets"]
                     ,input [ _type "checkbox"
                           , className "checkbox"
                           , checked $ true
                           , title "Mark as completed"
                             --  , onChange $ dispatch <<< ( const $ SetMap $ not (getter _._type state.term == MapTerm))
                           ] []
                   ]
                 , li []
                   [ span [] [text "Brevets"]
                     ,input [ _type "checkbox"
                           , className "checkbox"
                           , checked $ true
                           , title "Mark as completed"
                             --  , onChange $ dispatch <<< ( const $ SetMap $ not (getter _._type state.term == MapTerm))
                           ] []
                   ]
                 , li []
                   [ span [] [text "Others"]
                     ,input [ _type "checkbox"
                           , className "checkbox"
                           , checked $ true
                           , title "Mark as completed"
                             --  , onChange $ dispatch <<< ( const $ SetMap $ not (getter _._type state.term == MapTerm))
                           ] []
                   ]
                 ]

               ]
              , div []
                [ p []
                  [ text "Physical functions : common factor of side channel and fault attacks published in journal of cryptographic Enginering"
                  ]
                ]
               ]
             ]
           ]
         ]
    performAction :: PerformAction (console :: CONSOLE, dom :: DOM | eff) State props Action
    performAction SetGraph _ (State st) = void do
      gd <- liftEff $ getData st.fileReader
      modifyState \(State s) -> State $ s {graph = gd}

    performAction (SetFile f fn) _ _ = void do
      fr <- liftEff $ startProcessing f
      _ <- liftEff $ setupProgress fr (unsafeCoerce $ setP fr fn)
      modifyState \(State s) -> State $ s {fileReader = fr}

    performAction (SetProgress rs) _ _ = void do
      modifyState $ \(State s) -> State $ s {readyState = rs}


setP :: forall t89. FileReader -> (ReadyState -> Eff ( dom :: DOM | t89 ) Unit ) -> Eff ( dom :: DOM | t89 ) Unit
setP fr fn = do
  rs <- readyState fr
  fn rs
  pure unit
