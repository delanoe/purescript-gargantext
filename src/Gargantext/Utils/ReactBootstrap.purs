module Gargantext.Utils.ReactBootstrap where

import Effect (Effect)
import Reactix as R

import Gargantext.Prelude

import Gargantext.Utils.Reactix as R2

type OverlayTriggerProps =
  (
    overlay   :: R.Element
  , placement :: String
  , trigger   :: String
  )

type Props =
  (
  )

type AlertProps =
  ( dismissible :: Boolean
  , onClose     :: Effect Unit
  , variant     :: String
  )

type ContentProps =
  (
  )

type TitleProps =
  (
    "as" :: String
  )

foreign import alertCpt :: R.Component AlertProps
foreign import overlayTriggerCpt :: R.Component OverlayTriggerProps
foreign import popoverCpt :: R.Component Props
foreign import popoverContentCpt :: R.Component ContentProps
foreign import popoverTitleCpt :: R.Component TitleProps

alert :: R2.Component AlertProps
alert = R.rawCreateElement alertCpt

overlayTrigger :: R2.Component OverlayTriggerProps
overlayTrigger = R.rawCreateElement overlayTriggerCpt

popover :: R2.Component Props
popover = R.rawCreateElement popoverCpt

popoverContent :: R2.Component ContentProps
popoverContent = R.rawCreateElement popoverContentCpt

popoverTitle :: R2.Component TitleProps
popoverTitle = R.rawCreateElement popoverTitleCpt


-- example
-- example =
--     let popover = GUB.popover {} [
--               GUB.popoverTitle { "as": "h3" } [ H.text "hello title" ]
--             , GUB.popoverContent {} [ H.div {} [ H.text "content" ] ]
--            ]
--     in GUB.overlayTrigger { overlay: popover
--                           , placement: "right"
--                           , trigger: "click" } [
--       H.button { className: "btn btn-default" } [ H.text "Click me" ]
--       ]
