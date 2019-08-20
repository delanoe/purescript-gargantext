module Gargantext.Utils.Reactix
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, null, toMaybe)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import DOM.Simple.Element as Element
import DOM.Simple.Event as DE
import DOM.Simple as DOM
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import FFI.Simple ((...), defineProperty)
import React (ReactClass, ReactElement, Children, class IsReactElement, class ReactPropFields)
import React as React
import Reactix as R
import Reactix.DOM.HTML (ElemFactory)
import Reactix.React (createDOMElement)
import Reactix.SyntheticEvent as RE
import Thermite (Spec, simpleSpec, Render, defaultPerformAction)
import Unsafe.Coerce (unsafeCoerce)

newtype Point = Point { x :: Number, y :: Number }

type StateSetter a = (a -> a) -> Effect Unit

-- | Turns a ReactElement into aReactix Element
-- | buff (v.) to polish
buff :: ReactElement -> R.Element
buff = unsafeCoerce

-- | Turns a Reactix Element into a ReactElement.
-- | scuff (v.) to spoil the gloss or finish of.
scuff :: R.Element -> ReactElement
scuff = unsafeCoerce

class ToElement a where
  toElement :: a -> R.Element

instance toElementElement :: ToElement R.Element where
  toElement = identity

instance toElementReactElement :: ToElement ReactElement where
  toElement = buff

instance toElementArray :: ToElement a => ToElement (Array a) where
  toElement = R.fragment <<< map toElement

{-
instance isReactElementElement :: IsReactElement R.Element where
  toElement = scuff
-}

elSpec :: forall component props
        . R.IsComponent component props (Array R.Element)
       => component -> Spec {} (Record props) Void
elSpec cpt = simpleSpec defaultPerformAction render
  where
    render :: Render {} (Record props) Void
    render _ props _ children = [scuff $ R.createElement cpt props (buff <$> children)]

createElement' :: forall required given
                . ReactPropFields required given
               => ReactClass { children :: Children | required }
               -> Record given -> Array R.Element -> R.Element
createElement' reactClass props children =
  buff $ React.createElement reactClass props $ scuff <$> children

{-
instance isComponentReactClass
      :: R.IsComponent (ReactClass { children :: Children
                                   | props
                                   }) props (Array R.Element) where
  createElement reactClass props children =
    React.createElement reactClass props children
-}

mousePosition :: RE.SyntheticEvent DE.MouseEvent -> Point
mousePosition e = Point { x: RE.clientX e, y: RE.clientY e }

domMousePosition :: DE.MouseEvent -> Point
domMousePosition = mousePosition <<< unsafeCoerce
-- | This is naughty, it quietly mutates the input and returns it
named :: forall o. String -> o -> o
named = flip $ defineProperty "name"

overState :: forall t. (t -> t) -> R.State t -> Effect Unit
overState f (_state /\ setState) = setState f


select :: ElemFactory
select = createDOMElement "select"

menu :: ElemFactory
menu = createDOMElement "menu"

effToggler :: forall e. R.State Boolean -> EffectFn1 e Unit
effToggler (value /\ setValue) = mkEffectFn1 $ \e -> setValue $ const $ not value

unsafeEventValue :: forall event. event -> String
unsafeEventValue e = (unsafeCoerce e).target.value

nullRef :: forall t. R.Hooks (R.Ref (Nullable t))
nullRef = R.useRef null

nothingRef :: forall t. R.Hooks (R.Ref (Maybe t))
nothingRef = R.useRef Nothing

useLayoutEffect1' :: forall a. a -> (Unit -> Effect Unit) -> R.Hooks Unit
useLayoutEffect1' a f = R.useLayoutEffect1 a $ do
  liftEffect $ f unit
  pure $ pure unit

useLayoutRef :: forall a b. (a -> b) -> b -> R.Ref a -> R.Hooks (R.Ref b)
useLayoutRef fn init ref = do
  new <- R.useRef init
  let old = R.readRef ref
  useLayoutEffect1' old $ \_ -> R.setRef new (fn old)
  pure new

usePositionRef :: R.Ref (Nullable DOM.Element) -> R.Hooks (R.Ref (Maybe DOM.DOMRect))
usePositionRef = useLayoutRef (map Element.boundingRect <<< toMaybe) Nothing
