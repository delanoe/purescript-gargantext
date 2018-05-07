module GraphExplorer where

import Prelude hiding (div)

import Control.Monad.Aff (runAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Cont.Trans (lift)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import DOM (DOM)
import DOM.File.FileReader (fileReader, readAsText, result)
import DOM.File.Types (File, fileToBlob)
import React (ReactClass, createElement)
import React.DOM (button, button', div, form', input, li, li', menu, text, ul')
import React.DOM.Props (_id, _type, className, name, onChange, onClick, placeholder, style, value)
import Thermite (PerformAction, Render, Spec, modifyState, simpleSpec)

foreign import data GraphData :: Type

foreign import initialGraph :: GraphData

foreign import initialFile :: File

foreign import getFile :: forall e. e -> File

foreign import parseJSON :: forall eff a. a -> Eff eff GraphData

foreign import graphExplorerComponent :: ReactClass {graph :: GraphData, mode :: String}

foreign import logger :: forall a eff. a -> Eff eff Unit

newtype State = State {mode :: String, graph :: GraphData, file :: File}

data Action
  = SetGraph
  | SetFile File

initialState :: State
initialState = State {mode : "select", graph : initialGraph, file : initialFile}


reader :: forall eff. File -> Eff (console :: CONSOLE, dom :: DOM | eff) GraphData
reader f = do
  fr <- fileReader
  readAsText (fileToBlob f) fr
  res <- result fr
  --log $ show res
  let da = parseJSON res
  logger da
  da


spec :: forall eff props. Spec (console :: CONSOLE, dom :: DOM | eff) State props Action
spec = simpleSpec performAction render
  where
    render :: Render State props Action
    render d _ (State st) _ =
      [  div [className "row"] [
            div [className "col-md-12"]
            [ menu [_id "toolbar"]
              [ ul'
                [ li [style {display : "inline-block"}]
                  [ form'
                    [ input [_type "file", name "file", onChange (\e -> d $ SetFile $ getFile e)] []
                    , input [_type "button", value "submit", onClick \_ -> d SetGraph] []
                    ]
                  ]
                , li'
                  [ button [className "btn btn-success btn-sm"] [text "Change Type"]
                  ]
                , li'
                  [ button [className "btn btn-primary btn-sm"] [text "Change Level"]
                  ]
                , li'
                  [ form'
                    [ input [_type "text", name "query", placeholder "Select Topics"] []
                    , input [_type "submit", value "Search"] []
                    ]
                  ]
                , li'
                  [ button' [text "Screenshot"]]
                , li'
                  [ button' [text "Save"] -- TODO: Implement Save!
                  ]
                ]
              ]
            ]
           ]
         , div [className "row"]
           [ div [className "col-md-8"]
             [ div [style {border : "1px black solid", height: "90%"}]
               [ text "GraphExplorer here...."
               , createElement graphExplorerComponent { graph : st.graph
                                                      , mode : st.mode
                                                      } []
               ]
             ]
           , div [className "col-md-4"]
             [ div [_id "sidepanel", style {border : "1px black solid", height: "90%"}]
               [ text "SidePanel for contextual information"
               ]
             ]
           ]
         ]
    performAction :: PerformAction (console :: CONSOLE, dom :: DOM | eff) State props Action
    performAction SetGraph _ (State st) = void do
      gd <- liftEff $ reader st.file
      modifyState \(State s) -> State $ s {graph = gd}

    performAction (SetFile f) _ _ = void do
      modifyState \(State s) -> State $ s {file = f}
