module Gargantext.Components.EndsSummary
  -- (
  -- )
  where

import Reactix as R
import Reactix.DOM.HTML as H
import Data.Semigroup ((<>))
import Gargantext.Config (Ends)

endsSummary :: Ends -> R.Element
endsSummary ends = H.div {className: "text-info"} [ H.text text ]
  where text = "Connected to " <> ends.backend.name
