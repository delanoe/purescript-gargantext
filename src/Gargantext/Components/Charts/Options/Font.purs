module Gargantext.Components.Charts.Options.Font
       (
         TextStyle,
         ChartFontStyle(),
         chartFontStyle,
         ChartFontWeight(),
         chartFontWeight,
         Icon(),
         ImageURL(..),
         Shape(..),
         IconOptions(..),
         icon
       ) where

import Prelude (Unit, ($), (<<<), (<>))

import CSS (FontStyle(..), FontWeight(..), Prefixed(..), Value(..))
import Data.Generic (class Generic, gShow)
import Data.String (toLower)
import Gargantext.Components.Charts.Options.Color (ChartColor)
import Gargantext.Components.Charts.Options.Position (LeftRelativePosition, Position, TopRelativePosition)


type TextStyle =
  { color      :: ChartColor
  , fontStyle  :: ChartFontStyle
  , fontWeight :: ChartFontWeight
  , fontFamily :: String
  , fontSize   :: Int
  , align      :: Position LeftRelativePosition
  , verticalAlign :: Position TopRelativePosition
  , lineHeight    :: Position Unit
  , width         :: Position Unit
  , height        :: Position Unit
  , textBorderColor :: ChartColor
  , textBorderWidth :: Number
  , textShadowColor :: ChartColor
  , textShadowBlur  :: ChartColor
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
derive instance genericShape :: Generic Shape

data IconOptions = Shape Shape | Image ImageURL

icon :: IconOptions -> Icon
icon (Shape s) = Icon <<< toLower $ gShow s
icon (Image (ImageURL url)) = Icon $ "image://" <> url
