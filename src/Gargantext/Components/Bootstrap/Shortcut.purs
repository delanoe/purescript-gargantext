module Gargantext.Components.Bootstrap.Shortcut
  ( div', div_
  , h1', h1_
  , h2', h2_
  , h3', h3_
  , h4', h4_
  , h5', h5_
  , h6', h6_
  , span', span_
  , li', li_
  , b', b_
  , code', code_
  , label', label_
  , p', p_
  ) where

import Reactix as R
import Reactix.DOM.HTML as H

-- | Shorthand for using HTML <div> without writing its text node
div' :: forall r. Record r -> String -> R.Element
div' props content = H.div props [ H.text content ]

-- | Shorthand for using HTML <div> without writing its text node nor props
div_ :: String -> R.Element
div_ content = H.div {} [ H.text content ]


-- | Shorthand for using HTML <h1> without writing its text node
h1' :: forall r. Record r -> String -> R.Element
h1' props content = H.h1 props [ H.text content ]

-- | Shorthand for using HTML <h1> without writing its text node nor props
h1_ :: String -> R.Element
h1_ content = H.h1 {} [ H.text content ]

-- | Shorthand for using HTML <h2> without writing its text node
h2' :: forall r. Record r -> String -> R.Element
h2' props content = H.h2 props [ H.text content ]

-- | Shorthand for using HTML <h2> without writing its text node nor props
h2_ :: String -> R.Element
h2_ content = H.h2 {} [ H.text content ]

-- | Shorthand for using HTML <h3> without writing its text node
h3' :: forall r. Record r -> String -> R.Element
h3' props content = H.h3 props [ H.text content ]

-- | Shorthand for using HTML <h3> without writing its text node nor props
h3_ :: String -> R.Element
h3_ content = H.h3 {} [ H.text content ]

-- | Shorthand for using HTML <h4> without writing its text node
h4' :: forall r. Record r -> String -> R.Element
h4' props content = H.h4 props [ H.text content ]

-- | Shorthand for using HTML <h4> without writing its text node nor props
h4_ :: String -> R.Element
h4_ content = H.h4 {} [ H.text content ]

-- | Shorthand for using HTML <h5> without writing its text node
h5' :: forall r. Record r -> String -> R.Element
h5' props content = H.h5 props [ H.text content ]

-- | Shorthand for using HTML <h5> without writing its text node nor props
h5_ :: String -> R.Element
h5_ content = H.h5 {} [ H.text content ]

-- | Shorthand for using HTML <h6> without writing its text node
h6' :: forall r. Record r -> String -> R.Element
h6' props content = H.h6 props [ H.text content ]

-- | Shorthand for using HTML <h6> without writing its text node nor props
h6_ :: String -> R.Element
h6_ content = H.h6 {} [ H.text content ]

-- | Shorthand for using HTML <span> without writing its text node
span' :: forall r. Record r -> String -> R.Element
span' props content = H.span props [ H.text content ]

-- | Shorthand for using HTML <span> without writing its text node nor props
span_ :: String -> R.Element
span_ content = H.span {} [ H.text content ]

-- | Shorthand for using HTML <li> without writing its text node
li' :: forall r. Record r -> String -> R.Element
li' props content = H.li props [ H.text content ]

-- | Shorthand for using HTML <li> without writing its text node nor props
li_ :: String -> R.Element
li_ content = H.li {} [ H.text content ]

-- | Shorthand for using HTML <b> without writing its text node
b' :: forall r. Record r -> String -> R.Element
b' props content = H.b props [ H.text content ]

-- | Shorthand for using HTML <b> without writing its text node nor props
b_ :: String -> R.Element
b_ content = H.b {} [ H.text content ]

-- | Shorthand for using HTML <code> without writing its text node
code' :: forall r. Record r -> String -> R.Element
code' props content = H.code props [ H.text content ]

-- | Shorthand for using HTML <code> without writing its text node nor props
code_ :: String -> R.Element
code_ content = H.code {} [ H.text content ]

-- | Shorthand for using HTML <label> without writing its text node
label' :: forall r. Record r -> String -> R.Element
label' props content = H.label props [ H.text content ]

-- | Shorthand for using HTML <label> without writing its text node nor props
label_ :: String -> R.Element
label_ content = H.label {} [ H.text content ]

-- | Shorthand for using HTML <p> without writing its text node
p' :: forall r. Record r -> String -> R.Element
p' props content = H.p props [ H.text content ]

-- | Shorthand for using HTML <p> without writing its text node nor props
p_ :: String -> R.Element
p_ content = H.p {} [ H.text content ]
