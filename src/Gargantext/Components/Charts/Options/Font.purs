module Gargantext.Components.Charts.Options.Font
  ( ItemStyle
  , ItemStyleOptional
  , itemStyle
  , TextStyle
  , ChartFontStyle()
  , chartFontStyle
  , ChartFontWeight()
  , chartFontWeight
  , Icon()
  , ImageURL(..)
  , Shape(..)
  , IconOptions(..)
  , icon
  , Formatter
  , templateFormatter
  , Tooltip
  , TooltipOptional
  , mkTooltip
  , ToolBox
  , mkToolBox
  , Feature
  , DataView
  , MagicType
  , Save
  , Restore
  , Brush
  ) where

import Prelude (Unit, ($), (<<<), (<>))

import Data.Generic.Rep
import Data.Generic.Rep.Show (genericShow)
import CSS (FontStyle(..), FontWeight(..), Prefixed(..), Value(..))
import Data.String (toLower)
import Gargantext.Components.Charts.Options.Color (Color)
import Gargantext.Components.Charts.Options.Position (LeftRelativePosition, Position, TopRelativePosition)
import Gargantext.Types (class Optional)
import Unsafe.Coerce (unsafeCoerce)


type TextStyle =
  { color      :: Color
  , fontStyle  :: ChartFontStyle
  , fontWeight :: ChartFontWeight
  , fontFamily :: String
  , fontSize   :: Int
  , align      :: Position LeftRelativePosition
  , verticalAlign :: Position TopRelativePosition
  , lineHeight    :: Position Unit
  , width         :: Position Unit
  , height        :: Position Unit
  , textBorderColor :: Color
  , textBorderWidth :: Number
  , textShadowColor :: Color
  , textShadowBlur  :: Color
  , textShadowOffsetX :: Number
  , textShadowOffsetY :: Number
  }


newtype ChartFontStyle = ChartFontStyle String

chartFontStyle :: FontStyle -> ChartFontStyle
chartFontStyle (FontStyle (Value (Plain "italic"))) = ChartFontStyle "italic"
chartFontStyle (FontStyle (Value (Plain "oblique"))) = ChartFontStyle "oblique"
chartFontStyle _ = ChartFontStyle "normal"


newtype ChartFontWeight = ChartFontWeight String

chartFontWeight :: FontWeight -> ChartFontWeight
chartFontWeight (FontWeight (Value (Plain "bold"))) = ChartFontWeight "bold"
chartFontWeight (FontWeight (Value (Plain "bolder"))) = ChartFontWeight "bolder"
chartFontWeight (FontWeight (Value (Plain "lighter"))) = ChartFontWeight "lighter"
chartFontWeight  _ = ChartFontWeight "normal"


newtype Icon = Icon String

newtype ImageURL = ImageURL String

data Shape = Circle | Rect | RoundRect | Triangle | Diamond | Pin | Arrow
derive instance genericShape :: Generic Shape _

data IconOptions = Shape Shape | Image ImageURL

icon :: IconOptions -> Icon
icon (Shape s) = Icon <<< toLower $ genericShow s
icon (Image (ImageURL url)) = Icon $ "image://" <> url


data ItemStyle

type ItemStyleOptional =
  ( color :: Color
  )

itemStyle :: forall o. Optional o ItemStyleOptional => Record o -> ItemStyle
itemStyle = unsafeCoerce

data Formatter

templateFormatter :: String -> Formatter
templateFormatter = unsafeCoerce

-- TODO callbackFormatter :: (...) -> Formatter

data Tooltip

type TooltipOptional =
  ( trigger   :: String
    -- ^ Not all tooltips support triggers.
    -- Grid and legend tooltips : yes
    -- Series : no
  , show      :: Boolean
  , formatter :: Formatter
  )

-----------------------------------------------------------------
-- | ToolBox
mkToolBox :: ToolBox
mkToolBox = { feature: { dataView    : { show: true, readOnly : false, title : "Data"}
                       , saveAsImage : { show : true, pixelRatio : 10, title : "Image"}
                       --, magicType   : { show : true, "type" : ["line", "bar", "pie", "stack", "tiled"], title : "Type"}
                       --, restore     : {show : true, title : "Restore"}
                       --, brush : {"type" : ["rect", "polygon", "lineX", "lineY", "keep", "clear"]}
                     }
            , orient : "vertical"
          }


---------------------------------------
type ToolBox = { feature :: Feature
               , orient  :: String}
type Feature = { dataView :: DataView
               , saveAsImage   :: Save
               --, magicType :: MagicType
               --, restore :: Restore
               --, brush :: Brush
               }

---------------------------------------
type Save = { show :: Boolean
            , pixelRatio :: Int
            , title :: String
          }

type Restore = { show :: Boolean
               , title :: String}

type MagicType = { show :: Boolean
                 , "type" :: Array String  -- TODO use line bar types
                 , title :: String
               }
---------------------------------------

type DataView = { show :: Boolean
                , readOnly :: Boolean
                , title :: String
              }

type Brush = { "type" :: Array String }
---------------------------------------

mkTooltip :: forall o. Optional o TooltipOptional => Record o -> Tooltip
mkTooltip = unsafeCoerce
