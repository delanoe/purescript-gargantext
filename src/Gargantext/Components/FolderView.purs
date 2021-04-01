module Gargantext.Components.FolderView where

import Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff (Aff)
import Gargantext.Components.Forest.Tree.Node.Tools.FTree (FTree, LNode(..), NTree(..), fTreeID)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Prelude (compare, pure, ($), (<$>), (<>), Ordering)
import Gargantext.Routes (AppRoute(Home), SessionRoute(..), appPath, nodeTypeAppRoute)
import Gargantext.Sessions (Session, get, sessionId)
import Gargantext.Types (NodeType(..), SessionId, fldr)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H

here :: R2.Here
here = R2.here "Gargantext.Components.FolderView"

type Props =
  ( nodeId  :: Int
  , session :: Session
  )

data FolderStyle = FolderUp | FolderChild

folderViewLoad :: R2.Leaf Props
folderViewLoad props = R.createElement folderViewLoadCpt props []

folderViewLoadCpt :: R.Component Props
folderViewLoadCpt = here.component "folderViewLoadCpt" cpt where
  cpt {nodeId, session} _ = do
    useLoader {nodeId, session} loadFolders $
      \folders -> folderView {folders, nodeId, session}

type FolderViewProps = 
  ( 
    nodeId :: Int
  , folders:: FTree
  , session :: Session
  )

folderView :: Record FolderViewProps -> R.Element
folderView props = R.createElement folderViewCpt props []

folderViewCpt :: R.Component FolderViewProps
folderViewCpt = here.component "folderViewCpt" cpt where
  cpt {nodeId, session, folders: (NTree (LNode {parent_id: parentId}) (folders))} _ = do
    let sid = sessionId session 
    let foldersS = A.sortBy sortFolders folders
    let children = makeFolderElements foldersS sid
    let parent = makeParentFolder parentId sid

    pure $ H.div {className: "folders"} $ parent <> children

  makeFolderElements :: Array (NTree LNode) -> SessionId -> Array R.Element
  makeFolderElements foldersS sid = makeFolderElementsMap <$> foldersS where
    makeFolderElementsMap :: NTree LNode -> R.Element
    makeFolderElementsMap (NTree (LNode node) _) = folder {style: FolderChild, text: node.name, nodeId: node.id, nodeType: node.nodeType, sid: sid} []

  makeParentFolder :: Maybe Int -> SessionId -> Array R.Element
  makeParentFolder (Just parentId) sid =
    -- FIXME: The NodeType here should not be hardcoded to FolderPrivate but we currently can't get the actual NodeType
    -- without performing another API call. Also parentId is never being returned by this API even when it clearly exists
    [ folder {style: FolderUp, text: "..", nodeId: parentId, nodeType: FolderPrivate, sid: sid} [] ]
  makeParentFolder Nothing _ = []

  sortFolders :: FTree -> FTree -> Ordering
  sortFolders a b = compare (fTreeID a) (fTreeID b)


type FolderProps = 
  (
    style :: FolderStyle
  , text :: String
  , nodeType :: NodeType
  , nodeId :: Int
  , sid :: SessionId
  )

folder :: R2.Component FolderProps
folder = R.createElement folderCpt

folderCpt :: R.Component FolderProps
folderCpt = here.component "folderCpt" cpt where
  cpt {style, text, nodeId, sid, nodeType} _ = do
    pure $ H.a {className: "btn btn-primary", href: "/#/" <> getFolderPath nodeType sid nodeId}  [ H.i { className: icon style nodeType } []
                                                                   , H.br {}
                                                                   , H.text text]
  
  icon :: FolderStyle -> NodeType -> String
  icon FolderUp _ = "fa fa-folder-open"
  icon _ nodeType = fldr nodeType false

  getFolderPath :: NodeType -> SessionId -> Int -> String
  getFolderPath nodeType sid nodeId = appPath $ fromMaybe Home $ nodeTypeAppRoute nodeType sid nodeId

loadFolders :: Record Props -> Aff FTree
loadFolders {nodeId, session} = get session $ TreeFirstLevel (Just nodeId) ""
