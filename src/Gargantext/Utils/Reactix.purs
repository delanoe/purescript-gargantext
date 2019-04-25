module Gargantext.Utils.Reactix
  ( buff, scuff, nav, ul, li, a)
  where

import React ( ReactElement )
import Reactix as R
import Unsafe.Coerce ( unsafeCoerce )

-- | Turns a ReactElement into a Reactix Element
-- | buff (v.) to polish
buff :: ReactElement -> R.Element
buff = unsafeCoerce

-- | Turns a Reactix Element into a ReactElement.
-- | scuff (v.) to spoil the gloss or finish of. 
scuff :: R.Element -> ReactElement
scuff = unsafeCoerce

nav :: forall r. Record r -> Array R.Element -> R.Element
nav = R.createElement "nav"

ul :: forall r. Record r -> Array R.Element -> R.Element
ul = R.createElement "ul"

li :: forall r. Record r -> Array R.Element -> R.Element
li = R.createElement "li"

a :: forall r. Record r -> Array R.Element -> R.Element
a = R.createElement "a"

