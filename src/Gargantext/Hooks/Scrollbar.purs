module Gargantext.Hooks.Scrollbar
  ( useScrollbar
  ) where

import Gargantext.Prelude

import Effect (Effect)
import Reactix as R

foreign import disableScroll :: Effect Unit
foreign import enableScroll :: Effect Unit


type Output =
  ( disableScroll :: Effect Unit
  , enableScroll  :: Effect Unit
  )

useScrollbar :: R.Hooks (Record Output)
useScrollbar = pure
  { disableScroll
  , enableScroll
  }
