module Gargantext.Components.Nodes.Team where

import Gargantext.Prelude

import Gargantext.Components.App.Store (Boxes)
import Gargantext.Components.FolderView as FV
import Gargantext.Components.GraphQL.Team (TeamMember)
import Gargantext.Components.GraphQL.Endpoints (getTeam)
import Gargantext.Config.REST (AffRESTError, logRESTError)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Sessions (Session)
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
    useLoader { errorHandler
              , loader: loadTeam
              , path: { nodeId, session }
              , render: \team -> teamLayoutRows { boxes
                                                , team
                                                , nodeId
                                                , session
                                                }
              }
    where
      errorHandler = logRESTError here "teamLayout"

type TeamProps =
  ( boxes   :: Boxes
  , nodeId  :: ID
  , session :: Session
  , team    :: Array TeamMember )

teamLayoutRows :: R2.Leaf TeamProps
teamLayoutRows = R2.leafComponent teamLayoutRowsCpt

teamLayoutRowsCpt :: R.Component TeamProps
teamLayoutRowsCpt = here.component "teamLayoutRows" cpt where
  cpt { team } _ = do
    pure $ H.div {} $ map makeTeam team
    where
      makeTeam { username } = H.p {} [H.text username]

type LoadProps =
  (
    session :: Session,
    nodeId :: Int
  )


loadTeam :: Record LoadProps -> AffRESTError (Array TeamMember)
loadTeam { session, nodeId } = getTeam session nodeId
