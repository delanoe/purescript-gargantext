-- | The RangeSlider is a slider component with two knobs, allowing
-- | the user to specify both a minimum and maximum value to filter
-- | data by. It may be dragged with the mouse or moved with the
-- | keyboard like a regular slider component.  The RangeSlider is
-- | designed to let the user adjust in multiples of a provided
-- | epsilon (smallest difference)
module Gargantext.Components.RangeSlider where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Nullable (null)
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import DOM.Simple.Document (document)
import DOM.Simple.EventListener as EL
import DOM.Simple.Types (DOMRect, Element)
import DOM.Simple.Event as Event
import DOM.Simple.Console (log, log2)
import Data.Tuple.Nested ((/\))
import Reactix as R
import Reactix.DOM.HTML as H
import Reactix.SyntheticEvent as RE

import Gargantext.Utils.Math (roundToMultiple)
import Gargantext.Utils.Range as Range
import Gargantext.Utils.Reactix as R2
-- data Axis = X | Y

type NumberRange = Range.Closed Number
-- To avoid overloading the terms 'min' and 'max' here, we treat 'min'
-- and 'max' as being the bounds of the scale and 'low' and 'high' as
-- being the selected values
type Props =
  ( bounds :: NumberRange       -- The minimum and maximum values it is possible to select
  , initialValue :: NumberRange -- The user's selection of minimum and maximum values
  , epsilon :: Number           -- The smallest possible change (for mouse)
  , step :: Number              -- The 'standard' change (for keyboard)
  -- , axis :: Axis                -- Which direction to move in
  , width :: Number
  , height :: Number
  , onChange :: NumberRange -> Effect Unit )

rangeSlider :: Record Props -> R.Element
rangeSlider props = R.createElement rangeSliderCpt props []

data Knob = MinKnob | MaxKnob

data RangeUpdate = SetMin Number | SetMax Number

rangeSliderCpt :: R.Component Props
rangeSliderCpt = R.hooksComponent "RangeSlider" cpt
  where
    cpt props _ = do
      R.useEffect' $ do
        liftEffect $ log2 "Props: " props
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
      value /\ setValue <- R.useState' $ initialValue props
      let Range.Closed value' = value

      -- the knob we are currently in a drag for. set by mousedown on a knob
      dragKnob /\ setDragKnob <- R.useState' $ (Nothing :: Maybe Knob)
      -- the bounding box within which the mouse can drag
      dragScale <- R.useRef $ Nothing
      -- the handler functions for trapping mouse events, so they can be removed
      mouseMoveHandler <- (R.useRef $ Nothing) :: R.Hooks (R.Ref (Maybe (EL.Callback Event.MouseEvent)))
      mouseUpHandler <- (R.useRef $ Nothing) :: R.Hooks (R.Ref (Maybe (EL.Callback Event.MouseEvent)))
      let destroy = \_ ->
            do log "RangeSlider: Destroying event handlers"
               destroyEventHandler "mousemove" mouseMoveHandler
               destroyEventHandler "mouseup" mouseUpHandler
      R2.useLayoutEffect1' dragKnob $ \_ -> do
        case dragKnob of
          Just knob -> do
            let drag = (getDragScale knob scalePos lowPos highPos) :: Maybe NumberRange
            R.setRef dragScale drag
            let onMouseMove = EL.callback $ \(event :: Event.MouseEvent) ->
                  case reproject drag scalePos value (R2.domMousePosition event) of
                    Just val -> setKnob knob setValue value val
                    Nothing -> destroy unit
            let onMouseUp = EL.callback $ \(_event :: Event.MouseEvent) -> destroy unit
            log "RangeSlider: Creating event handlers"
            EL.addEventListener document "mousemove" onMouseMove
            EL.addEventListener document "mouseup" onMouseUp
          Nothing -> destroy unit
      pure $ H.div { className, aria }
        [ renderScale scaleElem props value'
        , renderKnob lowElem  value'.min ("Minimum value:" <> show value'.min) MinKnob setDragKnob
        , renderKnob highElem value'.max ("Maximum value:" <> show value'.max) MaxKnob setDragKnob
        ]
    className = "range-slider"
    aria = { label: "Range Slider Control. Expresses filtering data by a minimum and maximum value range through two slider knobs. Knobs can be adjusted with the arrow keys." }

destroyEventHandler
  :: forall e
  .  Event.IsEvent e
  => String -> R.Ref (Maybe (EL.Callback e)) -> Effect Unit
destroyEventHandler name ref = traverse_ destroy $ R.readRef ref
  where
    destroy handler = do
      EL.removeEventListener document name handler
      R.setRef ref Nothing

setKnob :: Knob -> ((NumberRange -> NumberRange) -> Effect Unit) -> NumberRange -> Number -> Effect Unit
setKnob knob setValue r val = setValue $ const $ setter knob r val
  where
    setter MinKnob = Range.withMin
    setter MaxKnob = Range.withMax

getDragScale :: Knob -> R.Ref (Maybe DOMRect) -> R.Ref (Maybe DOMRect) -> R.Ref (Maybe DOMRect) -> Maybe NumberRange
getDragScale knob scalePos lowPos highPos = do
  scale <- R.readRef scalePos
  low <- R.readRef lowPos
  high <- R.readRef highPos
  pure $ Range.Closed { min: min knob scale high, max: max knob scale low }
  where
    min MinKnob scale _ = scale.left
    min MaxKnob _ low = low.left
    max MinKnob _ high = high.left
    max MaxKnob scale _ = scale.right

renderScale ref {width,height} {min, max} =
   H.div { ref, className, width, height, aria } []
  where
    className = "scale"
    aria = { label: "Scale running from " <> show min <> " to " <> show max }

renderKnob ref val label knob set =
  H.div { ref, tabIndex, className, aria, onMouseDown } [ H.text (show val) ]
  where
    tabIndex = 0
    className = "knob"
    aria = { label }
    onMouseDown = mkEffectFn1 $ \_ -> set $ const $ Just knob

-- todo round to nearest epsilon
reproject :: Maybe NumberRange -> R.Ref (Maybe DOMRect) -> NumberRange -> R2.Point -> Maybe Number
reproject drag scale value (R2.Point mousePos) = do
  drag_ <- drag
  scale_ <- rectRange <$> R.readRef scale
  let normal = Range.normalise scale_ (Range.clamp drag_ mousePos.x)
  pure $ Range.projectNormal value normal
    
rectRange :: DOMRect -> NumberRange
rectRange rect = Range.Closed { min, max }
  where min = rect.left
        max = rect.right

initialValue :: Record Props -> NumberRange
initialValue props = roundRange props.epsilon props.bounds props.initialValue

round :: Number -> NumberRange -> Number -> Number
round epsilon bounds = roundToMultiple epsilon <<< Range.clamp bounds

roundRange :: Number -> NumberRange -> NumberRange -> NumberRange
roundRange epsilon bounds (Range.Closed initial) = Range.Closed { min, max }
  where min = round epsilon bounds initial.min
        max = round epsilon bounds initial.max
  
