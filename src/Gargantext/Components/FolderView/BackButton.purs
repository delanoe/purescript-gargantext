module Gargantext.Components.FolderView.BackButton where

import Gargantext.Prelude (Unit)

import Effect (Effect)
import Reactix as R
import Reactix.DOM.HTML as H

foreign import back :: Effect Unit

backButton :: R.Element
backButton = 
  H.button {
    className: "btn btn-primary"
  , on: {click: back}
  } [
    H.i { className: "fa fa-arrow-left"} []
  ]