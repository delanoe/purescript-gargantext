module Gargantext.Pages.Corpus.Doc.Facets.Terms.NgramsItem where

import Prelude

import Data.Newtype (class Newtype)
import React (ReactElement)
import React.DOM (input, span, td, text, tr)
import React.DOM.Props (_type, checked, className, onChange, style, title)
import Thermite (PerformAction, Render, Spec, modifyState, simpleSpec)
import Gargantext.Utils (getter, setter)

newtype State = State
  { term :: Term
  }

initialState :: State
initialState = State {term : Term {id : 10, term : "hello", occurrence : 10, _type : None, children : []}}

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

performAction :: PerformAction State {} Action
performAction (SetMap b)   _ _ = void do
  modifyState \(State s) -> State s {term = setter (_{_type = (if b then MapTerm else None)}) s.term}

performAction (SetStop b)   _ _ = void do
    modifyState \(State s) -> State s {term = setter (_{_type = (if b then StopTerm else None)}) s.term}

ngramsItemSpec :: Spec State {} Action
ngramsItemSpec = simpleSpec performAction render
  where
    render :: Render State {} Action
    render dispatch _ (State state) _ =
      [
        tr []
        [ td [] [ checkbox_map]
        , td [] [ checkbox_stop]
        , td [] [ dispTerm (getter _.term state.term)  (getter _._type state.term) ]
        , td [] [ text $ show $ getter _.occurrence state.term]
        ]
      ]
      where
        checkbox_map =
          input [ _type "checkbox"
                , className "checkbox"
                , checked $ getter _._type state.term == MapTerm
                , title "Mark as completed"
                , onChange $ dispatch <<< ( const $ SetMap $ not (getter _._type state.term == MapTerm))
                ]
        checkbox_stop =
          input
          [ _type "checkbox"
          , className "checkbox"
          , checked $ getter _._type state.term == StopTerm
          , title "Mark as completed"
          , onChange $ dispatch <<< ( const $ SetStop $ not (getter _._type state.term == StopTerm))
          ]


dispTerm :: String -> TermType -> ReactElement
dispTerm term MapTerm = span [style {color :"green"}] [text $ term]
dispTerm term StopTerm = span [style {color :"red", textDecoration : "line-through"}] [text $ term]
dispTerm term None = span [style {color :"black"}] [text term]
