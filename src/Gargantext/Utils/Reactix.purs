module Gargantext.Utils.Reactix
  where

import Prelude

import DOM.Simple.Event as DE
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, null, toMaybe)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import FFI.Simple ((...), defineProperty)
import React (ReactElement)
import Reactix as R
import Reactix.DOM.HTML (ElemFactory)
import Reactix.React (createDOMElement)
import Reactix.SyntheticEvent as RE
import Thermite (Spec, simpleSpec, Render, defaultPerformAction)
import Unsafe.Coerce (unsafeCoerce)
newtype Point = Point { x :: Number, y :: Number }

-- | Turns a ReactElement into a Reactix Element
-- | buff (v.) to polish
buff :: ReactElement -> R.Element
buff = unsafeCoerce

-- | Turns a Reactix Element into a ReactElement.
-- | scuff (v.) to spoil the gloss or finish of.
scuff :: R.Element -> ReactElement
scuff = unsafeCoerce

elSpec :: forall component props
        . R.IsComponent component props (Array R.Element)
       => component -> Spec {} (Record props) Void
elSpec cpt = simpleSpec defaultPerformAction render
  where
    render :: Render {} (Record props) Void
    render _ props _ children = [scuff $ R.createElement cpt props (buff <$> children)]

mousePosition :: RE.SyntheticEvent DE.MouseEvent -> Point
mousePosition e = Point { x: RE.clientX e, y: RE.clientY e }

-- | This is naughty, it quietly mutates the input and returns it
named :: forall o. String -> o -> o
named = flip $ defineProperty "name"

overState :: forall t. (t -> t) -> R.State t -> Effect Unit
overState f (_state /\ setState) = setState f


select :: ElemFactory
select = createDOMElement "select"

effToggler :: forall e. R.State Boolean -> EffectFn1 e Unit
effToggler (value /\ setValue) = mkEffectFn1 $ \e -> setValue $ const $ not value
