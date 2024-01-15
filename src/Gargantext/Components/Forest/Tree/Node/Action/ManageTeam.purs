module Gargantext.Components.Forest.Tree.Node.Action.ManageTeam where

import Gargantext.Prelude

import Data.Array (filter, null, (:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (runAff_)
import Effect.Class (liftEffect)
import Gargantext.Components.Forest.Tree.Node.Tools as Tools
import Gargantext.Components.GraphQL.Endpoints (deleteTeamMembership, getTeam)
import Gargantext.Components.GraphQL.Team (Team, TeamMember)
import Gargantext.Config.REST (AffRESTError, logRESTError)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Sessions (Session)
import Gargantext.Types (ID, NodeType)
import Gargantext.Utils.Reactix as R2
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
    useLoader { errorHandler
              , loader: loadTeam
              , path: { nodeId: id, session }
              , render: \team -> teamLayoutWrapper { team
                                                   , nodeId: id
                                                   , session
                                                   } []
              }
    where
      errorHandler = logRESTError here "teamLayout"

type TeamProps =
  ( nodeId  :: ID
  , session :: Session
  , team    :: Team
  )

teamLayoutWrapper :: R2.Component TeamProps
teamLayoutWrapper = R.createElement teamLayoutWrapperCpt
teamLayoutWrapperCpt :: R.Component TeamProps
teamLayoutWrapperCpt = here.component "teamLayoutWrapper" cpt where
  cpt {nodeId, session, team: {team_owner_username, team_members}} _ = do
    team_members <- T.useBox team_members
    error <- T.useBox Nothing

    pure $ teamLayoutRows {nodeId, session, team_members, error, team_owner_username}

type TeamRowProps =
  ( nodeId               :: ID
  , session              :: Session
  , team_members         :: T.Box (Array TeamMember)
  , error                :: T.Box (Maybe String)
  , team_owner_username :: String
  )

teamLayoutRows :: R2.Leaf TeamRowProps
teamLayoutRows = R2.leaf teamLayoutRowsCpt
teamLayoutRowsCpt :: R.Component TeamRowProps
teamLayoutRowsCpt = here.component "teamLayoutRows" cpt where
  cpt { team_members, nodeId, session, error, team_owner_username } _ = do
    team_members' <- T.useLive T.unequal team_members
    error' <- T.useLive T.unequal error
    
    pure $
      if null team_members' then
        H.div { style: {margin: "10px"}}
          [ H.h4 {} [H.text "Your team is empty, you can send some invitations."]]
      else
        Tools.panelNoFooter { mError: error' }
          (makeLeader team_owner_username : (map makeTeam team_members'))

    where
      makeTeam :: TeamMember -> R.Element
      makeTeam { username, shared_folder_id } =
        H.div {className: "from-group row"} [ H.div { className: "col-8" } [ H.text username ]
                                            , H.a { className: "text-danger col-2 fa fa-times"
                                                  , title: "Remove user from team"
                                                  , type: "button"
                                                  , on: {click: submit shared_folder_id }
                                                  } []
                                            ]

      makeLeader username =
        H.div {className: "from-group row"} [ H.div { className: "col-8"} [ H.text username ]
                                            , H.p { className: "col-2"} [ H.text "owner"]
                                            ]

      submit sharedFolderId _ = do
        runAff_ callback $ saveDeleteTeam { session, nodeId, sharedFolderId }

      callback res =
        case res of
          Left _ -> do
            _ <- liftEffect $ T.write (Just "Only the Team owner can remove users") error
            pure unit
          Right val ->
            case val of
              Left _ -> do
                pure unit
              Right r -> do
                T.modify_ (filter (\tm -> tm.shared_folder_id /= r)) team_members
                T.write_ Nothing error

-------------------------------------------------------------

type LoadProps =
  (
    session :: Session,
    nodeId  :: Int
  )


loadTeam :: Record LoadProps -> AffRESTError Team
loadTeam { session, nodeId } = getTeam session nodeId

type DeleteProps =
  (
    session        :: Session,
    nodeId         :: Int,
    sharedFolderId :: Int
  )


saveDeleteTeam ∷ Record DeleteProps -> AffRESTError Int
saveDeleteTeam { session, nodeId, sharedFolderId } = deleteTeamMembership session sharedFolderId nodeId
