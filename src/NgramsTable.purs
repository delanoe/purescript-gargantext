module NgramsTable where

import CSS.TextAlign (center, textAlign)
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Data.Array (fold, toUnfoldable)
import Data.Either (Either(..))
import Data.Lens (Lens', Prism', lens, over, prism)
import Data.List (List(..))
import Data.Tuple (Tuple(..), uncurry)
import Network.HTTP.Affjax (AJAX)
import NgramsItem as NI
import Prelude hiding (div)
import React.DOM hiding (style)
import React.DOM.Props (_id, className, scope, style)
import Thermite (PerformAction, Spec, _render, focus, foreach, modifyState, withState)


newtype State = State
  { items :: List NI.State
  }

initialState :: State
initialState = State { items : toUnfoldable [NI.initialState]}

data Action
  = NoOp
  | ItemAction Int NI.Action

_itemsList :: Lens' State (List NI.State)
_itemsList = lens (\(State s) -> s.items) (\(State s) v -> State s { items = v })

_ItemAction :: Prism' Action (Tuple Int NI.Action)
_ItemAction = prism (uncurry ItemAction) \ta ->
  case ta of
    ItemAction i a -> Right (Tuple i a)
    _ -> Left ta

performAction :: forall eff props. PerformAction ( console :: CONSOLE , ajax    :: AJAX, dom     :: DOM | eff ) State props Action
performAction _ _ _ = void do
  modifyState id

tableSpec :: forall eff props .Spec eff State props Action -> Spec eff State props Action
tableSpec = over _render \render dispatch p s c ->
  [div [className "container1"]
     [
       div [className "jumbotron"]
       [ div [className "row"]
         [ div [className "panel panel-default"]
           [
             div [className "panel-heading"]
             [ h2 [className "panel-title", style {textAlign : "center"}]
               [ span [className "glyphicon glyphicon-hand-down"] []
               , text "Extracted Terms"
               ]
             , div [className "savediv pull-left", style { margin :"1.5em 0 0 0", padding :"0 1em 0 0"}]
               [ span [className "needsaveicon glyphicon glyphicon-import"] []
               , button [_id "ImportListOrSaveAll", className "btn btn-warning", style {fontSize : "120%"}]
                 [ text "Import a Termlist"
                 ]
               ]
             , div []
               [
               ]
             ]

          ,  table [ className "table able table-bordered"]
           [ thead [ className "tableHeader table-bordered"]
             [ tr []
               [ th [ scope "col"] [ text "Map" ]
               , th [ scope "col"] [ text "Stop"]
               , th [ scope "col"] [ text "Terms"]
               , th [ scope "col"] [ text "Occurences (nb)" ]
               ]
             ]
           , tbody [] $  render dispatch p s c
           ]
         ]
       ]
     ]
  ]
 ]
ngramsTableSpec :: forall props eff . Spec (console::CONSOLE, ajax::AJAX, dom::DOM | eff) State props Action
ngramsTableSpec =  container $ fold
    [  tableSpec $ withState \st ->
        focus _itemsList _ItemAction $
          foreach \_ -> NI.ngramsItemSpec
   ]

container :: forall eff state props action. Spec eff state props action -> Spec eff state props action
container = over _render \render d p s c ->
  [ div [ className "container" ] $
    (render d p s c)
  ]
