module GraphExplorer where

import Prelude hiding (div)

import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import DOM (DOM)
import DOM.File.FileReader (fileReader, readAsText, readyState, result)
import DOM.File.FileReader.ReadyState (ReadyState(..))
import DOM.File.Types (File, FileReader, fileToBlob)
import React (ReactClass, createElement)
import React.DOM (button, button', div, form', input, li, li', menu, text, ul')
import React.DOM.Props (_id, _type, className, disabled, name, onChange, onClick, placeholder, style, value)
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
            div [className "col-md-12"]
            [ menu [_id "toolbar"]
              [ ul'
                [ li [style {display : "inline-block"}]
                  [ form'
                    [ input [_type "file", name "file", onChange (\e -> d $ SetFile (getFile e) (unsafeCoerce $ d <<< SetProgress))] []
                    , input [_type "button", value "submit", onClick \_ -> d SetGraph, disabled (st.readyState /= DONE)] []
                    -- , text $ show st.readyState
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
               [ createElement graphExplorerComponent { graph : st.graph
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
