-- | The RangeSlider is a slider component with two knobs, allowing
-- | the user to specify both a minimum and maximum value to filter
-- | data by. It may be dragged with the mouse or moved with the
-- | keyboard like a regular slider component.  The RangeSlider is
-- | designed to let the user adjust in multiples of a provided
-- | epsilon (smallest difference)
module Gargantext.Components.RangeSlider where

import Prelude
import Reactix as R
import Data.Traversable (traverse_)
import Gargantext.Utils.Reactix as R2
import DOM.Simple.Document (document)
import DOM.Simple.Element as Element
import DOM.Simple.Types (DOMRect, Element)
import Data.Tuple.Nested ((/\))
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Gargantext.Utils.Math (roundToMultiple)
import Gargantext.Utils.Range as Range
import Data.Maybe (Maybe(..))
import Data.Nullable (null)
import Effect (Effect)
import Reactix.DOM.HTML as H
-- data Axis = X | Y

type NumberRange = Range.Closed Number
-- To avoid overloading the terms 'min' and 'max' here, we treat 'min'
-- and 'max' as being the bounds of the scale and 'low' and 'high' as
-- being the selected values
type Props =
  { bounds :: NumberRange       -- The minimum and maximum values it is possible to select
  , initialValue :: NumberRange -- The user's selection of minimum and maximum values
  , epsilon :: Number           -- The smallest possible change (for mouse)
  , step :: Number              -- The 'standard' change (for keyboard)
  -- , axis :: Axis                -- Which direction to move in
  , width :: Number
  , height :: Number
  , onChange :: NumberRange -> Effect Unit }

rangeSlider :: Props -> R.Element
rangeSlider = R.createElement rangeSliderCpt

data Knob = MinKnob | MaxKnob

data RangeUpdate = SetMin Number | SetMax Number

rangeSliderCpt :: Props -> R.Component Props
rangeSliderCpt = R.hooksComponent "RangeSlider" cpt
  where
    cpt props _ = do
      -- scale bar
      scaleElem <- R.useRef null -- dom ref
      scalePos <- R2.usePositionRef scaleElem
      -- low knob
      lowElem <- R.useRef null -- a dom ref to the low knob
      lowPos <- R2.usePositionRef lowElem
      -- high knob
      highElem <- R.useRef null -- a dom ref to the high knob
      highPos <- R2.usePositionRef highElem
      -- The value of the user's selection
      value /\ setValue <- R.useState $ \_ -> pure $ initialValue props
      let Range.Closed value' = value

      -- the knob we are currently in a drag for. set by mousedown on a knob
      dragKnob /\ setDragKnob <- R.useState $ \_ -> pure Nothing
      -- the bounding box within which the mouse can drag
      dragScale <- R.useRef $ Nothing
      -- the handler functions for trapping mouse events, so they can be removed
      mouseMoveHandler <- R.useRef $ Nothing
      mouseUpHandler <- R.useRef $ Nothing
      
      R.useLayoutEffect1 dragKnob $ \_ -> do
        case dragKnob of
          Just knob -> do
            let drag = getDragScale knob scalePos lowPos highPos
            R.setRef dragScale drag
            let onMouseMove = mkEffectFn1 $ \event ->
              setKnob knob setValue value $ reproject drag scalePos value $ R2.mousePosition event
            let onMouseUp = mkEffectFn1 $ \event ->
              destroyEventHandler "mousemove" mouseMoveHandler *>
              destroyEventHandler "mouseup" mouseUpHandler
            Element.addEventListener document "mousemove" onMouseMove
            Element.addEventListener document "mouseup" onMouseUp
          Nothing -> do
            destroyEventHandler "mousemove" mouseMoveHandler
            destroyEventHandler "mouseup" mouseUpHandler
      H.div { className, aria }
        [ renderScale scaleElem props value'
        , renderKnob lowElem  value'.min ("Minimum value:" <> show value'.min) MinKnob setDragKnob
        , renderKnob highElem value'.max ("Maximum value:" <> show value'.max) MaxKnob setDragKnob
        ]
    className = "range-slider"
    aria = { label: "Range Slider Control. Expresses filtering data by a minimum and maximum value range through two slider knobs. Knobs can be adjusted with the arrow keys." }

destroyEventHandler :: forall e. String -> R.Ref (Maybe (e -> EffectFn1 e Unit)) -> Effect Unit
destroyEventHandler name ref = traverse_ destroy $ R.readRef ref
  where destroy handler = Element.removeEventListener document name handler *> R.setRef ref Nothing

setKnob :: Knob -> (Range.Closed Number -> Effect Unit) -> Range.Closed Number -> Number -> Effect Unit
setKnob knob setValue r val = setValue $ setter knob r val
  where
    setter MinKnob = Range.withMin
    setter MaxKnob = Range.withMax

getDragScale :: Knob -> R.Ref (Maybe DOMRect) -> R.Ref (Maybe DOMRect) -> R.Ref (Maybe DOMRect) -> Range.Closed Number
getDragScale knob scalePos lowPos highPos = Range.Closed { min: min knob, max: max knob }
  where
    scale = R.readRef scalePos
    low = R.readRef lowPos
    high = R.readRef highPos
    min MinKnob = scale.left
    min MaxKnob = low.left
    max MinKnob = high.left
    max MaxKnob = scale.right

renderScale ref {width,height} {min, max} =
   H.div { ref, className, width, height, aria } []
  where
    className = "scale"
    aria = { label: "Scale running from " <> show min <> " to " <> show max }

renderKnob ref val label knob set =
  H.div { ref, tabindex, className, aria, onMouseDown } [ H.text (show val) ]
  where
    tabindex = 0
    className = "knob"
    aria = { label }
    onMouseDown = mkEffectFn1 $ \_ -> set knob

-- todo round to nearest epsilon
reproject :: Range.Closed Number -> Range.Closed Number -> R2.Point -> Number
reproject drag scale value mousePos = Range.projectNormal value normal
  where
    normal = Range.normalise scale (Range.clamp drag mousePos.x)

initialValue :: Props -> NumberRange
initialValue props = roundRange props.epsilon props.bounds props.initialValue
  
round :: Number -> NumberRange -> Number -> Number
round epsilon bounds = roundToMultiple epsilon <<< Range.clamp bounds

roundRange :: Number -> NumberRange -> NumberRange -> NumberRange
roundRange epsilon bounds (Range.Closed initial) = Range.Closed { min, max }
  where min = round epsilon bounds initial.min
        max = round epsilon bounds initial.max
  
