module Gargantext.Components.Forest.Tree.Node.Action.Share where

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)
import Reactix as R
import Reactix.DOM.HTML as H
import Simple.JSON as JSON
import Simple.JSON.Generics as JSONG
import Toestand as T

import Gargantext.Prelude

import Gargantext.Components.Forest.Tree.Node.Action.Types (Action)
import Gargantext.Components.Forest.Tree.Node.Action.Types as Action
import Gargantext.Components.Forest.Tree.Node.Tools as Tools
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree (subTreeView, SubTreeParamsIn)
import Gargantext.Config.REST (RESTError)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, post)
import Gargantext.Types (ID)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Action.Share"

------------------------------------------------------------------------
shareReq :: Session -> ID -> ShareNodeParams -> Aff (Either RESTError ID)
shareReq session nodeId =
  post session $ GR.NodeAPI GT.Node (Just nodeId) "share"

shareAction :: String -> Action
shareAction username = Action.ShareTeam username


------------------------------------------------------------------------
data ShareNodeParams = ShareTeamParams   { username :: String }
               | SharePublicParams { node_id  :: Int    }
derive instance Eq ShareNodeParams
derive instance Generic ShareNodeParams _
instance JSON.ReadForeign ShareNodeParams where readImpl = JSONG.untaggedSumRep
instance JSON.WriteForeign ShareNodeParams where writeImpl = JSON.writeImpl <<< show
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
      isOpen <- T.useBox true
      pure $ Tools.panel
                [ Tools.textInputBox { boxAction: shareAction
                                     , boxName: "Share"
                                     , dispatch
                                     , id
                                     , isOpen
                                     , text: "username" } []
                ] (H.div {} [])
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
