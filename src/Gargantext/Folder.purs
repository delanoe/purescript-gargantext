module Gargantext.Folder where

import Prelude
import React.DOM (div, h1, text)
import Thermite (Render, Spec, simpleSpec, defaultPerformAction)

render :: Render {} {} Void
render dispatch _ state _ = [h1 [] [text "Folder"]]

layoutFolder :: Spec {} {} Void
layoutFolder = simpleSpec defaultPerformAction render

