module Gargantext.Components.Charts.Options.Color
       ( Color
       , stringColor
       , cssColor
       , transparent
       , red
       , blue
       , magenta
       , violet
       , black
       ) where

import Prelude

import CSS as CSS
import Color (rgba)
import Unsafe.Coerce (unsafeCoerce)

data Color

stringColor :: String -> Color
stringColor = unsafeCoerce

cssColor :: CSS.Color -> Color
cssColor = stringColor <<< CSS.toHexString

transparent :: Color
transparent = cssColor $ rgba 255 255 255 0.0

red :: Color
red = stringColor "red"

blue :: Color
blue = stringColor "blue"

magenta :: Color
magenta = stringColor "magenta"

violet :: Color
violet = cssColor CSS.violet

black :: Color
black = stringColor "black"
