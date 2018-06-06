module NgramsTable where

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
import React.DOM (div, table, tbody, text, th, thead, tr)
import React.DOM.Props (className, scope)
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
  [table [ className "table able table-bordered"]
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
