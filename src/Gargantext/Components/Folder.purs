module Gargantext.Components.Folder where

import Reactix as R
import Reactix.DOM.HTML as H

-- TODO : get REST informations

folder :: {} -> R.Element
folder props = R.createElement folderCpt props []

folderCpt :: R.Component ()
folderCpt = R.staticComponent "G.C.Folder.folder" cpt
  where
    cpt _ _ =
      R.fragment
      [ H.h1 {} [ H.text "Folder" ]
      , H.text "Some description of the folder here" ]

