module Gargantext.Components.Bootstrap.Label
  ( label'
  , label_
  ) where

import Reactix as R
import Reactix.DOM.HTML as H

-- | Shorthand for using HTML <label> without writing its text node
label' :: forall r. Record r -> String -> R.Element
label' props content = H.label props [ H.text content ]

-- | Shorthand for using HTML <label> without writing its text node nor props
label_ :: String -> R.Element
label_ content = H.label {} [ H.text content ]
