module Gargantext.Folder where

import Prelude
import React.DOM (div, h1, text, p)
import Thermite (Render, Spec, simpleSpec, defaultPerformAction)

-- TODO : get REST informations

render :: Render {} {} Void
render dispatch _ state _ = [ h1 [] [text "Folder"] 
                            , text "Some description of the folder here"
                            ]

layoutFolder :: Spec {} {} Void
layoutFolder = simpleSpec defaultPerformAction render

