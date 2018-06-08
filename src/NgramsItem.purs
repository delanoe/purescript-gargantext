module NgramsItem where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import DOM (DOM)
import Data.Array (foldr)
import Data.Newtype (class Newtype)
import Network.HTTP.Affjax (AJAX)
import React (ReactElement)
import React.DOM (button, input, li, span, td, text, tr, ul)
import React.DOM.Props (_type, checked, className, onChange, onClick, style, title)
import Thermite (PerformAction, Render, Spec, modifyState, simpleSpec)
import Utils (getter, setter)

newtype State = State
  { term :: Term
  , editingGroup :: Boolean
  }

derive instance newtypeState :: Newtype State _

initialState :: State
initialState = State
  { term : Term {id : 10, term : "hello", occurrence : 10, _type : None, children : []}
  , editingGroup : false
  }

newtype Term = Term {id :: Int, term :: String, occurrence :: Int, _type :: TermType, children :: Array Term}

derive instance newtypeTerm :: Newtype Term _

data TermType = MapTerm | StopTerm | None

derive instance eqTermType :: Eq TermType

instance showTermType :: Show TermType where
  show MapTerm = "MapTerm"
  show StopTerm = "StopTerm"
  show None = "None"

data Action
  = SetMap Boolean
  | SetStop Boolean
  | ModifyGroup
  | AddToGroup
  | RemoveFromGroup

performAction :: forall eff props. PerformAction ( console :: CONSOLE , ajax    :: AJAX, dom     :: DOM | eff ) State props Action
performAction (SetMap b)   _ _ = void do
  modifyState \(State s) -> State s {term = setter (_{_type = _type, children = ch s.term}) s.term}
    where
      _type = if b then MapTerm else None
      ch (Term t) = map (setter (_{_type = _type})) t.children

performAction (SetStop b)   _ _ = void do
    modifyState \(State s) -> State s {term = setter (_{_type = _type, children = ch s.term}) s.term}
    where
      _type = if b then StopTerm else None
      ch (Term t) = map (setter (_{_type = _type})) t.children

performAction _   _ _ = void do
    modifyState id

ngramsItemSpec :: forall props eff . Spec (console::CONSOLE, ajax::AJAX, dom::DOM | eff) State props Action
ngramsItemSpec = simpleSpec performAction render
  where
    render :: Render State props Action
    render dispatch _ (State state) _ =
      [
        tr []
        [ td [] [ checkbox_map]
        , td [] [ checkbox_stop]
        , td []
          [ dispTerm (getter _.term state.term)  (getter _._type state.term)
          , dispTermChildren state.term
          , if state.editingGroup then button [onClick $ dispatch <<< (const AddToGroup)][text "Add To Group"] else button [ onClick $ dispatch <<< (const ModifyGroup)][text "Modify Group"]
          ]
        , td [] [ text $ show $ termTotal state.term]
        ]
      ]
      where
        checkbox_map =
          input [ _type "checkbox"
                , className "checkbox"
                , checked $ getter _._type state.term == MapTerm
                , title "Mark as map term"
                , onChange $ dispatch <<< ( const $ SetMap $ not (getter _._type state.term == MapTerm))
                ] []
        checkbox_stop =
          input
          [ _type "checkbox"
          , className "checkbox"
          , checked $ getter _._type state.term == StopTerm
          , title "Mark as Stop Term"
          , onChange $ dispatch <<< ( const $ SetStop $ not (getter _._type state.term == StopTerm))
          ] []

dispTerm :: String -> TermType -> ReactElement
dispTerm term MapTerm = span [style {color :"green"}] [text $ term]
dispTerm term StopTerm = span [style {color :"red", textDecoration : "line-through"}] [text $ term]
dispTerm term None = span [style {color :"black"}] [text term]

termTotal :: Term -> Int
termTotal term = foldr (+) 0 ary
  where
    ary = [getter _.occurrence term] <> (map (getter _.occurrence) (getter _.children term))

dispTermChildren :: Term -> ReactElement
dispTermChildren term = ul [] $ map childTerm (getter _.children term)
  where
    childTerm :: Term -> ReactElement
    childTerm child = li [] [dispTerm (getter _.term child) (getter _._type child)]
