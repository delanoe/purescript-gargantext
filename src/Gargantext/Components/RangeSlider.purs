-- | The RangeSlider is a slider component with two knobs, allowing
-- | the user to specify both a minimum and maximum value to filter
-- | data by. It may be dragged with the mouse or moved with the
-- | keyboard like a regular slider component.  The RangeSlider is
-- | designed to let the user adjust in multiples of a provided
-- | epsilon (smallest difference)
module Gargantext.Components.RangeSlider where

import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as DN
import Data.Number.Format as DNF
import Data.Nullable (Nullable, null)
import Data.Traversable (traverse_)
import DOM.Simple as DOM
import DOM.Simple.Document (document)
import DOM.Simple.Event as Event
import DOM.Simple.EventListener as EL
import DOM.Simple (DOMRect)
import Effect (Effect)
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Prelude

import Gargantext.Utils.Math (roundToMultiple)
import Gargantext.Utils.Range as Range
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.RangeSlider"
-- data Axis = X | Y

type Bounds = Range.NumberRange
type Epsilon = Number

-- To avoid overloading the terms 'min' and 'max' here, we treat 'min'
-- and 'max' as being the bounds of the scale and 'low' and 'high' as
-- being the selected values
type Props =
  ( bounds :: Bounds                  -- The minimum and maximum values it is possible to select
  , initialValue :: Range.NumberRange -- The user's selection of minimum and maximum values
  , epsilon :: Number                 -- The smallest possible change (for mouse)
  , step :: Number                    -- The 'standard' change (for keyboard)
  -- , axis :: Axis                   -- Which direction to move in
  , width :: Number
  , height :: Number
  , onChange :: Range.NumberRange -> Effect Unit )

rangeSlider :: Record Props -> R.Element
rangeSlider props = R.createElement rangeSliderCpt props []

data Knob = MinKnob | MaxKnob
derive instance Generic Knob _
instance Eq Knob where
  eq = genericEq

data RangeUpdate = SetMin Number | SetMax Number

rangeSliderCpt :: R.Component Props
rangeSliderCpt = here.component "rangeSlider" cpt
  where
    cpt props _ = do
      -- rounding precision (i.e. how many decimal digits are in epsilon)
      let precision = fromMaybe 0 $ fromNumber $ max 0.0 $ - DN.floor $ (DN.log props.epsilon) / DN.ln10

      -- scale bar
      scaleElem <- (R.useRef null) :: R.Hooks (R.Ref (Nullable DOM.Element)) -- dom ref
      -- scale sel bar
      scaleSelElem <- (R.useRef null) :: R.Hooks (R.Ref (Nullable DOM.Element)) -- dom ref
      -- low knob
      lowElem <- (R.useRef null) :: R.Hooks (R.Ref (Nullable DOM.Element)) -- a dom ref to the low knob
      -- high knob
      highElem <- (R.useRef null) :: R.Hooks (R.Ref (Nullable DOM.Element)) -- a dom ref to the high knob
      -- The value of the user's selection
      value <- T.useBox $ initialValue props
      value' <- T.useLive T.unequal value

      -- the knob we are currently in a drag for. set by mousedown on a knob
      dragKnob <- T.useBox (Nothing :: Maybe Knob)
      dragKnob' <- T.useLive T.unequal dragKnob

      -- the handler functions for trapping mouse events, so they can be removed
      mouseMoveHandler <- (R.useRef $ Nothing) :: R.Hooks (R.Ref (Maybe (EL.Callback Event.MouseEvent)))
      mouseUpHandler <- (R.useRef $ Nothing) :: R.Hooks (R.Ref (Maybe (EL.Callback Event.MouseEvent)))
      let destroy = \_ -> do
            destroyEventHandler "mousemove" mouseMoveHandler
            destroyEventHandler "mouseup" mouseUpHandler
            R.setRef mouseMoveHandler $ Nothing
            R.setRef mouseUpHandler $ Nothing

      R2.useLayoutEffect1' dragKnob' $ \_ -> do
        let scalePos = R2.readPositionRef scaleElem
        let lowPos = R2.readPositionRef lowElem
        let highPos = R2.readPositionRef highElem

        case dragKnob' of
          Just knob -> do
            let drag = (getDragScale knob scalePos lowPos highPos) :: Maybe Range.NumberRange

            let onMouseMove = EL.callback $ \(event :: Event.MouseEvent) -> do
                  case reproject drag scalePos props.bounds props.epsilon (R2.domMousePosition event) of
                    Just val -> do
                      setKnob knob value value' val
                      props.onChange $ knobSetter knob value' val
                    Nothing -> destroy unit
            let onMouseUp = EL.callback $ \(_event :: Event.MouseEvent) -> do
                  --props.onChange $ knobSetter knob value val
                  T.write_ Nothing dragKnob
                  destroy unit
            EL.addEventListener document "mousemove" onMouseMove
            EL.addEventListener document "mouseup" onMouseUp
            R.setRef mouseMoveHandler $ Just onMouseMove
            R.setRef mouseUpHandler $ Just onMouseUp
          Nothing -> destroy unit
      pure $ H.div { className, aria }
        [ renderScale scaleElem props value'
        , renderScaleSel scaleSelElem props value'
        , renderKnob MinKnob lowElem  value' props.bounds dragKnob precision
        , renderKnob MaxKnob highElem value' props.bounds dragKnob precision
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

setKnob :: Knob -> T.Box Range.NumberRange -> Range.NumberRange -> Number -> Effect Unit
setKnob knob value r val = T.write_ (knobSetter knob r val) value

knobSetter :: Knob -> Range.NumberRange -> Number -> Range.NumberRange
knobSetter MinKnob = Range.withMin
knobSetter MaxKnob = Range.withMax

getDragScale :: Knob -> Maybe DOMRect -> Maybe DOMRect -> Maybe DOMRect -> Maybe Range.NumberRange
getDragScale knob scalePos lowPos highPos = do
  scale <- scalePos
  low <- lowPos
  high <- highPos
  pure $ Range.Closed { min: min knob scale low, max: max knob scale high }
  where
    min MinKnob scale _ = scale.left
    min MaxKnob _ low = low.left
    max MinKnob _ high = high.left
    max MaxKnob scale _ = scale.right

renderScale :: R.Ref (Nullable DOM.Element) -> Record Props -> Range.NumberRange -> R.Element
renderScale ref {width,height} (Range.Closed {min, max}) =
   H.div { ref, className, width, height, aria } []
  where
    className = "range-slider__scale"
    aria = { label: "Scale running from " <> show min <> " to " <> show max }

renderScaleSel :: R.Ref (Nullable DOM.Element) -> Record Props -> Range.NumberRange -> R.Element
renderScaleSel ref props (Range.Closed {min, max}) =
    H.div { ref, className, style} []
  where
    className = "range-slider__scale-sel"
    style = {left: computeLeft, width: computeWidth}
    percOffsetMin = Range.normalise props.bounds min
    percOffsetMax = Range.normalise props.bounds max
    computeLeft = formatter $ 100.0 * percOffsetMin
    computeWidth = formatter $ 100.0 * (percOffsetMax - percOffsetMin)
    formatter n = (DNF.toStringWith (DNF.fixed 0) n) <> "%"


renderKnob :: Knob -> R.Ref (Nullable DOM.Element) -> Range.NumberRange -> Bounds -> T.Box (Maybe Knob) -> Int -> R.Element
renderKnob knob ref (Range.Closed value) bounds set precision =
  H.div { ref, tabIndex, className, aria, on: { mouseDown: onMouseDown }, style } [
      H.div { className: "range-slider__placeholder" }
        [
          H.text $ DNF.toStringWith (DNF.precision precision) val
        ]
  ]
  where
    tabIndex = 0
    className = "range-slider__knob"
    aria = { label: labelPrefix knob <> "value: " <> show val }
    labelPrefix MinKnob = "Minimum "
    labelPrefix MaxKnob = "Maximum "
    onMouseDown _ = T.write_ (Just knob) set
    percOffset = Range.normalise bounds val
    style = { left: (show $ 100.0 * percOffset) <> "%" }
    val = case knob of
      MinKnob -> value.min
      MaxKnob -> value.max

-- TODO round to nearest epsilon
reproject :: Maybe Range.NumberRange -> Maybe DOMRect -> Bounds -> Epsilon -> R2.Point -> Maybe Number
reproject drag scalePos bounds epsilon (R2.Point mousePos) = do
  drag_ <- drag
  scale_ <- rectRange <$> scalePos
  let normal = Range.normalise scale_ (Range.clamp drag_ mousePos.x)
  let val = Range.projectNormal bounds normal
  pure $ round epsilon bounds val

rectRange :: DOMRect -> Range.NumberRange
rectRange rect = Range.Closed { min, max }
  where min = rect.left
        max = rect.right

initialValue :: Record Props -> Range.NumberRange
initialValue props = roundRange props.epsilon props.bounds props.initialValue

round :: Epsilon -> Bounds -> Number -> Number
round epsilon bounds = roundToMultiple epsilon <<< Range.clamp bounds

roundRange :: Epsilon -> Bounds -> Range.NumberRange -> Range.NumberRange
roundRange epsilon bounds (Range.Closed initial) = Range.Closed { min, max }
  where min = round epsilon bounds initial.min
        max = round epsilon bounds initial.max
