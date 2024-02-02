module Gargantext.Components.Forest.Tree.Node.Action.ManageTeam where

import Gargantext.Prelude

import Data.Array (cons, filter, null, (:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (runAff_)
import Effect.Class (liftEffect)
import Gargantext.Components.Forest.Tree.Node.Tools as Tools
import Gargantext.Components.GraphQL.Endpoints (deleteTeamMembership, getTeam)
import Gargantext.Components.GraphQL.Team (Team, TeamMember)
import Gargantext.Config.REST (AffRESTError, FrontendError(EC_403__user_not_authorized))
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Sessions (Session)
import Gargantext.Types (ID, FrontendError(FRESTError), NodeType)
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
  cpt { id, session } _ = do
    useLoader { errorHandler: Nothing
              , herePrefix: R2.herePrefix here "[actionManageTeam]"
              , loader: loadTeam
              , path: { nodeId: id, session }
              , render: \team -> teamLayoutWrapper { team
                                                   , nodeId: id
                                                   , session
                                                   } []
              }

type TeamProps =
  ( nodeId  :: ID
  , session :: Session
  , team    :: Team
  )

teamLayoutWrapper :: R2.Component TeamProps
teamLayoutWrapper = R.createElement teamLayoutWrapperCpt
teamLayoutWrapperCpt :: R.Component TeamProps
teamLayoutWrapperCpt = here.component "teamLayoutWrapper" cpt where
  cpt { nodeId, session, team: { team_owner_username, team_members } } _ = do
    error <- T.useBox Nothing    
    team_members <- T.useBox team_members

    pure $ teamLayoutRows {nodeId, session, team_members, error, team_owner_username}

type TeamRowProps =
  ( error   :: T.Box (Maybe String)
  , nodeId               :: ID
  , session              :: Session
  , team_members         :: T.Box (Array TeamMember)
  , team_owner_username  :: String
  )

teamLayoutRows :: R2.Leaf TeamRowProps
teamLayoutRows = R2.leaf teamLayoutRowsCpt
teamLayoutRowsCpt :: R.Component TeamRowProps
teamLayoutRowsCpt = here.component "teamLayoutRows" cpt where
  cpt { error, nodeId, session, team_members, team_owner_username } _ = do
    team_members' <- T.useLive T.unequal team_members
    error' <- T.useLive T.unequal error
    
    pure $
      if null team_members' then
        H.div { style: {margin: "10px"} }
          [ H.h4 {} [H.text "Your team is empty, you can send some invitations."] ]
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

      callback res = do
        case res of
          Left err -> do
            here.log2 "[callback] error forking runAff" err
            T.write_ (Just "Error forking 'runAff_'") error
          Right val -> do
            here.log2 "[callback] val" val
            case val of
              -- Left (EC_403__user_not_authorized { msg }) -> do
              --   liftEffect $ T.write_ (Just msg) error
              Left err -> do
                T.write_ (Just $ show err) error
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
