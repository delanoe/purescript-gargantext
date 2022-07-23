module Gargantext.Components.Nodes.Team where

import Gargantext.Prelude

import Effect.Class (liftEffect)
import Gargantext.Components.App.Store (Boxes)
import Gargantext.Components.FolderView as FV
import Gargantext.Components.GraphQL.Endpoints (getTeam, deleteTeamMembership)
import Gargantext.Components.GraphQL.Team (TeamMember)
import Gargantext.Config.REST (AffRESTError, logRESTError)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Sessions (Session)
import Gargantext.Types (ID)
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

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
  cpt props _ = do
    pure $ R.fragment [
      FV.folderView props
    , teamLayoutMain props
    ]

teamLayoutMain :: R2.Leaf Props
teamLayoutMain = R2.leafComponent teamLayoutMainCpt

teamLayoutMainCpt :: R.Component Props
teamLayoutMainCpt = here.component "teamLayoutMain" cpt where
  cpt { nodeId, session, boxes } _ = do
    reload <- T.useBox T2.newReload
    _ <- T.useLive T.unequal reload

    useLoader { errorHandler
              , loader: loadTeam
              , path: { nodeId, session }
              , render: \team -> teamLayoutRows { boxes
                                                , team
                                                , nodeId
                                                , session
                                                , reload
                                                }
              }
    where
      errorHandler = logRESTError here "teamLayout"

type TeamProps =
  ( boxes   :: Boxes
  , nodeId  :: ID
  , session :: Session
  , team    :: Array TeamMember
  , reload  :: T.Box T2.Reload )

teamLayoutRows :: R2.Leaf TeamProps
teamLayoutRows = R2.leafComponent teamLayoutRowsCpt

teamLayoutRowsCpt :: R.Component TeamProps
teamLayoutRowsCpt = here.component "teamLayoutRows" cpt where
  cpt { team, nodeId, session, reload } _ = do
    pure $ H.div {} $ map makeTeam team
    where
      makeTeam { username, shared_folder_id } = H.div {} [H.text username, H.button {className: "btn btn-danger", on: {click: submit shared_folder_id}} [ H.text "remove" ]]
      
      submit sharedFolderId = do
        _ <- saveDeleteTeam { session, nodeId, sharedFolderId }
        liftEffect $ T2.reload reload

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
