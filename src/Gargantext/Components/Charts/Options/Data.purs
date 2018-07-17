module Gargantext.Components.Charts.Options.Data where

import Gargantext.Components.Charts.Options.Font (TextStyle, Icon)

type DataN =
  { name :: String
  , icon :: Icon
  , textStyle :: TextStyle
  }

type DataV =
  { value :: String
  , textStyle :: TextStyle
  }

type DataS =
  { name :: String
  , value :: Number
  }
