module Gargantext.Components.Forest.Tree.Node.Action.ManageTeam where

import Gargantext.Prelude

import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Gargantext.Components.Forest.Tree.Node.Tools as Tools
import Gargantext.Components.GraphQL.Endpoints (deleteTeamMembership, getTeam)
import Gargantext.Components.GraphQL.Team (TeamMember)
import Gargantext.Config.REST (AffRESTError, logRESTError)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Sessions (Session)
import Gargantext.Types (ID, NodeType)
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

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
  cpt {id, session} _ = do
    reload <- T.useBox T2.newReload
    _ <- T.useLive T.unequal reload

    useLoader { errorHandler
              , loader: loadTeam
              , path: { nodeId: id, session }
              , render: \team -> teamLayoutRows { team
                                                , nodeId: id
                                                , session
                                                , reload
                                                }
              }
    where
      errorHandler = logRESTError here "teamLayout"

type TeamProps =
  ( nodeId  :: ID
  , session :: Session
  , team    :: Array TeamMember
  , reload  :: T.Box T2.Reload )

teamLayoutRows :: R2.Leaf TeamProps
teamLayoutRows = R2.leafComponent teamLayoutRowsCpt

teamLayoutRowsCpt :: R.Component TeamProps
teamLayoutRowsCpt = here.component "teamLayoutRows" cpt where
  cpt { team, nodeId, session, reload } _ = do
    pure $ Tools.panel (map makeTeam team) (H.div {} [])
    where
      makeTeam { username, shared_folder_id } = H.div {className: "from-group row"} [ H.div { className: "col-8" } [ H.text username ]
                                                                                    , H.a { className: "text-danger col-2 fa fa-times"
                                                                                          , title: "Remove user from team"
                                                                                          , type: "button"
                                                                                          , on: {click: submit shared_folder_id} 
                                                                                          } []
                                                                                    ]
      
      submit sharedFolderId _ = do
        launchAff_ $ saveDeleteTeam { session, nodeId, sharedFolderId }
        liftEffect $ T2.reload reload

-------------------------------------------------------------

type LoadProps =
  (
    session :: Session,
    nodeId :: Int
  )


loadTeam :: Record LoadProps -> AffRESTError (Array TeamMember)
loadTeam { session, nodeId } = getTeam session nodeId

type DeleteProps =
  (
    session :: Session,
    nodeId :: Int,
    sharedFolderId :: Int
  )


saveDeleteTeam ∷ Record DeleteProps -> AffRESTError Int
saveDeleteTeam { session, nodeId, sharedFolderId } = deleteTeamMembership session sharedFolderId nodeId
