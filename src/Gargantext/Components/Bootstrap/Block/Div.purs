module Gargantext.Components.Bootstrap.Div (div', div_) where

import Reactix as R
import Reactix.DOM.HTML as H

-- | Shorthand for using HTML <div> without writing its text node
div' :: forall r. Record r -> String -> R.Element
div' props content = H.div props [ H.text content ]

-- | Shorthand for using HTML <div> without writing its text node nor props
div_ :: String -> R.Element
div_ content = H.div {} [ H.text content ]
