module Gargantext.Pages.Folder where

import Prelude

import Thermite (Render, Spec, defaultPerformAction, simpleSpec)


projets :: Spec {} {} Void
projets = simpleSpec defaultPerformAction render
  where
    render :: Render {} {} Void
    render dispatch _ state _ =
      []
