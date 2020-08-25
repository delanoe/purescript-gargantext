module Gargantext.Components.Forest.Tree.Node.Action.Share where

import Data.Argonaut as Argonaut
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Gargantext.Components.Forest.Tree.Node.Action (Action)
import Gargantext.Components.Forest.Tree.Node.Action as Action
import Gargantext.Components.Forest.Tree.Node.Tools as Tools
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree (subTreeView, SubTreeParamsIn)
import Gargantext.Prelude (class Eq, class Show, bind, pure)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, post)
import Gargantext.Types (ID)
import Gargantext.Types as GT
import Gargantext.Utils.Argonaut (genericSumDecodeJson, genericSumEncodeJson)
import Prelude (($))
import Reactix as R
import Reactix.DOM.HTML as H

------------------------------------------------------------------------
shareReq :: Session -> ID -> ShareNodeParams -> Aff ID
shareReq session nodeId =
  post session $ GR.NodeAPI GT.Node (Just nodeId) "share"

shareAction :: String -> Action
shareAction username = Action.ShareTeam username

------------------------------------------------------------------------
textInputBox :: Record Tools.TextInputBoxProps -> R.Element
textInputBox  = Tools.textInputBox

------------------------------------------------------------------------
data ShareNodeParams = ShareTeamParams   { username :: String }
               | SharePublicParams { node_id  :: Int    }

derive instance eqShareNodeParams :: Eq ShareNodeParams

derive instance genericShareNodeParams :: Generic ShareNodeParams _

instance showShareNodeParams :: Show ShareNodeParams where
  show = genericShow

instance decodeJsonShareNodeParams :: Argonaut.DecodeJson ShareNodeParams where
  decodeJson = genericSumDecodeJson

instance encodeJsonShareNodeParams :: Argonaut.EncodeJson ShareNodeParams where
  encodeJson = genericSumEncodeJson


------------------------------------------------------------------------
shareNode :: Record SubTreeParamsIn -> R.Element
shareNode p = R.createElement shareNodeCpt p []

shareNodeCpt :: R.Component SubTreeParamsIn
shareNodeCpt = R.hooksComponent "G.C.F.T.N.A.M.shareNode" cpt
  where
    cpt p@{dispatch, subTreeParams, id, nodeType, session, handed} _ = do
      action@(valAction /\ setAction) :: R.State Action <- R.useState' (Action.SharePublic {params: Nothing})

      let button = case valAction of
              Action.SharePublic {params} -> case params of
                Just val -> Tools.submitButton (Action.SharePublic {params: Just val}) dispatch
                Nothing -> H.div {} []
              _   -> H.div {} []

      pure $ Tools.panel [ subTreeView { action
                                       , dispatch
                                       , id
                                       , nodeType
                                       , session
                                       , subTreeParams
                                       , handed
                                       }
              ] button

