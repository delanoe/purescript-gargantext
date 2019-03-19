module Gargantext.Components.Charts.Options.Data where

import Record.Unsafe (unsafeSet)
import Unsafe.Coerce (unsafeCoerce)
import Gargantext.Types (class Optional)
import Gargantext.Components.Charts.Options.Font (TextStyle, Icon, ItemStyle)

type DataLegend =
  { name :: String
  , icon :: Icon
  , textStyle :: TextStyle
  }

type DataAxis =
  { value :: String
  , textStyle :: TextStyle
  }

type RequiredData v o =
  { value :: v
  | o
  }

type OptionalData =
  ( name       :: String
  , symbolSize :: Number
  , itemStyle  :: ItemStyle
    -- ^ the style setting about single data point(bubble).
  , label      :: { show :: Boolean }
  )

type DataSerie v = RequiredData v OptionalData

dataSerie :: forall v o. Optional o OptionalData => RequiredData v o -> DataSerie v
dataSerie = unsafeCoerce

dataSerieV :: forall v. v -> DataSerie v
dataSerieV value = dataSerie {value}

type DataD1 = DataSerie Number
type DataD2 = DataSerie (Array Number)

dataD1 :: forall o. Optional o OptionalData => Record o -> Number -> DataD1
dataD1 o x = unsafeCoerce (unsafeSet "value" x o)

dataD2 :: forall o. Optional o OptionalData => Record o -> Number -> Number -> DataD2
dataD2 o x y = unsafeCoerce (unsafeSet "value" [x,y] o)
