module Gargantext.Utils.ReactBootstrap where

import Reactix as R

import Gargantext.Prelude

type OverlayTriggerProps =
  (
    overlay   :: R.Element
  , placement :: String
  , trigger   :: String
  )

type Props =
  (
  )

type ContentProps =
  (
  )

type TitleProps =
  (
    "as" :: String
  )

foreign import overlayTriggerCpt :: R.Component OverlayTriggerProps
foreign import popoverCpt :: R.Component Props
foreign import popoverContentCpt :: R.Component ContentProps
foreign import popoverTitleCpt :: R.Component TitleProps

overlayTrigger :: Record OverlayTriggerProps -> Array R.Element -> R.Element
overlayTrigger = R.rawCreateElement overlayTriggerCpt

popover :: Record Props -> Array R.Element -> R.Element
popover = R.rawCreateElement popoverCpt

popoverContent :: Record ContentProps -> Array R.Element -> R.Element
popoverContent = R.rawCreateElement popoverContentCpt

popoverTitle :: Record TitleProps -> Array R.Element -> R.Element
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
