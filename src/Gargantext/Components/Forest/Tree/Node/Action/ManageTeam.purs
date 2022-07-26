module Gargantext.Components.Forest.Tree.Node.Action.ManageTeam where

import Gargantext.Prelude

import Gargantext.Components.Forest.Tree.Node.Tools (panel)
import Gargantext.Sessions (Session(..))
import Gargantext.Types (ID, NodeType)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Action.ManageTeam"

type ActionManageTeam = (
  id       :: ID
, nodeType :: NodeType
, session  :: Session
)

actionManageTeam :: R2.Component ActionManageTeam
actionManageTeam = R.createElement actionManageTeamCpt

actionManageTeamCpt :: R.Component ActionManageTeam
actionManageTeamCpt = here.component "actionManageTeam" cpt where
  cpt _ _ = do
    pure $ H.text "todo"
