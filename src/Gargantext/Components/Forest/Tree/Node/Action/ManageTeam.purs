module Gargantext.Components.Forest.Tree.Node.Action.ManageTeam where

import Gargantext.Prelude

import Data.Array (filter)
import Data.Either (Either(..))
import Effect.Aff (runAff_)
import Effect.Class (liftEffect)
import Gargantext.Components.Forest.Tree.Node.Tools as Tools
import Gargantext.Components.GraphQL.Endpoints (deleteTeamMembership, getTeam)
import Gargantext.Components.GraphQL.Team (TeamMember)
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
  , team    :: (Array TeamMember)
  )

teamLayoutWrapper :: R2.Component TeamProps
teamLayoutWrapper = R.createElement teamLayoutWrapperCpt

teamLayoutWrapperCpt :: R.Component TeamProps
teamLayoutWrapperCpt = here.component "teamLayoutWrapper" cpt where
  cpt {nodeId, session, team} _ = do
    teamS <- T.useBox team
    team' <- T.useLive T.unequal teamS
    error <- T.useBox ""
    error' <- T.useLive T.unequal error

    pure $ teamLayoutRows {nodeId, session, team: teamS, team', error, error'}

type TeamRowProps =
  ( nodeId  :: ID
  , session :: Session
  , team    :: T.Box (Array TeamMember)
  , error   :: T.Box String 
  , team'   :: Array TeamMember
  , error'  :: String
  )

teamLayoutRows :: R2.Leaf TeamRowProps
teamLayoutRows = R2.leafComponent teamLayoutRowsCpt

teamLayoutRowsCpt :: R.Component TeamRowProps
teamLayoutRowsCpt = here.component "teamLayoutRows" cpt where
  cpt { team, nodeId, session, error, team', error' } _ = do
    
    pure $ Tools.panel (map makeTeam team') (H.div {} [H.text error'])
    where
      makeTeam :: TeamMember -> R.Element
      makeTeam { username, shared_folder_id } = H.div {className: "from-group row"} [ H.div { className: "col-8" } [ H.text username ]
                                                                                    , H.a { className: "text-danger col-2 fa fa-times"
                                                                                          , title: "Remove user from team"
                                                                                          , type: "button"
                                                                                          , on: {click: submit shared_folder_id } 
                                                                                          } []
                                                                                    ]
      
      submit sharedFolderId _ = do
        runAff_ callback $ saveDeleteTeam { session, nodeId, sharedFolderId }

      callback res =
        case res of
          Left _ -> do
            _ <- liftEffect $ T.write "Only the Team owner can remove users" error
            pure unit
          Right val ->
            case val of
              Left _ -> do
                pure unit
              Right r -> do
                T.write_ (filter (\tm -> tm.shared_folder_id /= r) team') team

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
