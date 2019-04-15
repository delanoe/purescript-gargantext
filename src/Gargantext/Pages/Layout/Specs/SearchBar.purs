module Gargantext.Pages.Layout.Specs.SearchBar
  (State, Action(..), initialState, performAction, renderSpec) where

import Data.Lens (Lens', lens, over, (^.), (.~))
import Data.Newtype as N
import Effect.Class.Console (log)
import Effect.Class                                    (liftEffect)
import React.DOM (button, div, i, input, li, text, ul)
import React.DOM.Props (_type, className, onChange, onClick, placeholder, style)
import Thermite (PerformAction, Render, Spec, modifyState, simpleSpec)
import Unsafe.Coerce (unsafeCoerce)

import Gargantext.Prelude
import Gargantext.Components.Modals.Modal              (modalShow)

type State' = { open :: Boolean, searchTerm :: String }

newtype State = State State'

derive instance newtypeState :: N.Newtype State _

initialState :: State
initialState = State { open: false, searchTerm: "" }

data Action =
    ToggleOpen
  | SetSearchTerm String
  | PerformSearch

performAction :: PerformAction State {} Action
performAction ToggleOpen _ st = void $ do
  let new = st ^. _open
  let msg = "Toggled open from " <> show new
  liftEffect $ log msg
  modifyState $ over _open not
performAction (SetSearchTerm term) _ _ = void $ do
  liftEffect $ log $ "Search term set " <> term
  modifyState $ _searchTerm .~ term
performAction PerformSearch _ _ = void $ do
  liftEffect $ log "Search performed"
  liftEffect $ modalShow "addCorpus"

render :: Render State {} Action
render dispatch _ state _ = [ expander ] <> (draw $ state ^. _open)
  where
    draw true  = [ searchbar ]
    draw false = [ ]
    go = button [onClick \e -> dispatch PerformSearch, className "btn btn-primary"]
                [text "Enter"]
    expander = ul [ className "nav navbar pull-left" ]
                  [ li [ onClick \e -> dispatch ToggleOpen, style { color: "#039BE5" } ]
                       [ i [ className "material-icons md-36", style { marginTop: "-5px" } ]
                           [ text "control_point" ] ] ]
    search = input [ className   "search-query"
                   , placeholder "Query, URL or FILE (works with Firefox or Chromium browsers)"
                   , _type "text"
                   , style { height: "35px", width: "400px" }
                   , onChange \e -> dispatch $ SetSearchTerm (unsafeCoerce e).target.value
                   ]
    searchbar = ul [ className "nav navbar pull-left" ]
                   [ div [className "navbar-form"] [ search, go ] ]


-- TODO:
 -- render differently based on whether we are open or not
 -- tidy up css
 -- subtle css animation

renderSpec :: Spec State {} Action
renderSpec = simpleSpec performAction render


----------------------------

overState :: (State' -> State') -> State -> State
overState = N.over State

_open :: Lens' State Boolean
_open = lens (_.open <<< N.unwrap) $ \s o -> overState (_ { open = o }) s

_searchTerm :: Lens' State String
_searchTerm = lens (_.searchTerm <<< N.unwrap) $ \s t -> overState (_ { searchTerm = t }) s

