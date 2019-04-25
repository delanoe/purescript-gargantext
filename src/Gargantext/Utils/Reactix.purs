module Gargantext.Utils.Reactix
  ( WithChildren, HooksTree, HooksLeaf, buff, scuff, nav, ul, li, a, div )
  where

import React ( ReactElement )
import Reactix as R
import Unsafe.Coerce ( unsafeCoerce )

-- | A convenience for adding `children` to a list of props
type WithChildren p = ( children :: R.Children | p )

type HooksTree m p = R.MonadHooks m => Record p -> Array R.Element -> m R.Element
type HooksLeaf m p = R.MonadHooks m => Record p -> m R.Element

-- | Turns a ReactElement into a Reactix Element
-- | buff (v.) to polish
buff :: ReactElement -> R.Element
buff = unsafeCoerce

-- | Turns a Reactix Element into a ReactElement.
-- | scuff (v.) to spoil the gloss or finish of. 
scuff :: R.Element -> ReactElement
scuff = unsafeCoerce

div :: forall r. Record r -> Array R.Element -> R.Element
div = R.createDOMElement "div"

nav :: forall r. Record r -> Array R.Element -> R.Element
nav = R.createDOMElement "nav"

ul :: forall r. Record r -> Array R.Element -> R.Element
ul = R.createDOMElement "ul"

li :: forall r. Record r -> Array R.Element -> R.Element
li = R.createDOMElement "li"

a :: forall r. Record r -> Array R.Element -> R.Element
a = R.createDOMElement "a"

