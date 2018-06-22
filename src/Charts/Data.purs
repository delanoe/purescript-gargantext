module Charts.Data where

import Charts.Font (TextStyle, Icon)
import Prelude ((<<<))
import Unsafe.Coerce (unsafeCoerce)

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
