module Gargantext.Utils.Reactix
  where

import Prelude
import Data.Maybe ( Maybe(..) )
import Data.Nullable ( Nullable, null, toMaybe )
import Data.Traversable ( traverse_ )
import Data.Tuple ( Tuple(..) )
import Data.Tuple.Nested ( (/\) )
import DOM.Simple.Event as DE
import FFI.Simple ( (...), defineProperty )
import React ( ReactElement )
import Reactix as R
import Reactix.SyntheticEvent as RE
import Unsafe.Coerce ( unsafeCoerce )
newtype Point = Point { x :: Number, y :: Number }

-- | Turns a ReactElement into a Reactix Element
-- | buff (v.) to polish
buff :: ReactElement -> R.Element
buff = unsafeCoerce

-- | Turns a Reactix Element into a ReactElement.
-- | scuff (v.) to spoil the gloss or finish of. 
scuff :: R.Element -> ReactElement
scuff = unsafeCoerce

mousePosition :: RE.SyntheticEvent DE.MouseEvent -> Point
mousePosition e = Point { x: RE.clientX e, y: RE.clientY e }

-- | This is naughty, it quietly mutates the input and returns it
named :: forall o. String -> o -> o
named = flip $ defineProperty "name"
