module Gargantext.Components.Forest.Tree.Node.Action.Share where

import Gargantext.Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Gargantext.Components.Forest.Tree.Node.Action.Types (Action)
import Gargantext.Components.Forest.Tree.Node.Action.Types as Action
import Gargantext.Components.Forest.Tree.Node.Tools as Tools
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree (subTreeView, SubTreeParamsIn)
import Gargantext.Config.REST (AffRESTError)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, post)
import Gargantext.Types (ID)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.SimpleJSON as GUSJ
import Reactix as R
import Reactix.DOM.HTML as H
import Simple.JSON as JSON
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Action.Share"

------------------------------------------------------------------------
shareReq :: Session -> ID -> ShareNodeParams -> AffRESTError ID
shareReq session nodeId =
  post session $ GR.NodeAPI GT.Node (Just nodeId) "share"

shareAction :: String -> Action
shareAction username = Action.ShareTeam username


------------------------------------------------------------------------
data ShareNodeParams = ShareTeamParams   { username :: String }
               | SharePublicParams { node_id  :: Int    }
derive instance Eq ShareNodeParams
derive instance Generic ShareNodeParams _
instance JSON.ReadForeign ShareNodeParams where readImpl = GUSJ.taggedSumRep
instance JSON.WriteForeign ShareNodeParams where
  writeImpl (ShareTeamParams { username }) = JSON.writeImpl { "type": "ShareTeamParams"
                                                            , username }
  writeImpl (SharePublicParams { node_id }) = JSON.writeImpl { "type": "SharePublicParams"
                                                             , node_id }
instance Show ShareNodeParams where show = genericShow

------------------------------------------------------------------------
type ShareNode =
  ( id :: ID
  , dispatch :: Action -> Aff Unit )

shareNode :: R2.Component ShareNode
shareNode = R.createElement shareNodeCpt
shareNodeCpt :: R.Component ShareNode
shareNodeCpt = here.component "shareNode" cpt
  where
    cpt { dispatch, id } _ = do
      username' /\ username <- R2.useBox' ""

      pure $ Tools.panel
                [ Tools.inviteInputBox { boxAction: shareAction
                                        , boxName: "Share"
                                        , dispatch
                                        , id
                                        , text: "username"
                                        , username } []
                ] (H.div {} [H.text username'])
------------------------------------------------------------------------
publishNode :: R2.Component SubTreeParamsIn
publishNode = R.createElement publishNodeCpt
publishNodeCpt :: R.Component SubTreeParamsIn
publishNodeCpt = here.component "publishNode" cpt
  where
    cpt { boxes, dispatch, id, nodeType, session, subTreeParams } _ = do
      action <- T.useBox (Action.SharePublic { params: Nothing })
      action' <- T.useLive T.unequal action

      let button = case action' of
              Action.SharePublic { params } -> case params of
                Just val -> Tools.submitButton (Action.SharePublic {params: Just val}) dispatch
                Nothing -> H.div {} []
              _   -> H.div {} []

      pure $ Tools.panel
        [ subTreeView { action
                      , boxes
                      , dispatch
                      , id
                      , nodeType
                      , session
                      , subTreeParams
                      } []
        ] button
