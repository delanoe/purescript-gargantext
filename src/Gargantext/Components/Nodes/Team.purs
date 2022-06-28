module Gargantext.Components.Nodes.Team where

import Gargantext.Prelude

import Gargantext.Components.App.Store (Boxes)
import Gargantext.Sessions (Session(..))
import Gargantext.Types (ID)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Team"

type Props =
  ( boxes   :: Boxes
  , nodeId  :: ID
  , session :: Session )

teamLayout :: R2.Leaf Props
teamLayout = R2.leafComponent teamLayoutCpt

teamLayoutCpt :: R.Component Props
teamLayoutCpt = here.component "teamLayout" cpt where
  cpt _ _ = do
    pure $ H.text "todo"
