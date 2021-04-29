module Gargantext.Components.FolderView where

import Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Gargantext.Components.Forest.Tree.Node.Tools.FTree (FTree, LNode(..), NTree(..), fTreeID)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Prelude (Ordering, Unit, compare, pure, ($), (<$>), (<>))
import Gargantext.Routes (AppRoute(Home), SessionRoute(..), appPath, nodeTypeAppRoute)
import Gargantext.Sessions (Session, get, sessionId)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H

foreign import back :: Effect Unit

here :: R2.Here
here = R2.here "Gargantext.Components.FolderView"

type Props =
  ( nodeId  :: Int
  , session :: Session
  , backFolder :: Boolean
  )

data FolderStyle = FolderUp | FolderChild

folderView :: R2.Leaf Props
folderView props = R.createElement folderViewCpt props []

folderViewCpt :: R.Component Props
folderViewCpt = here.component "folderViewCpt" cpt where
  cpt {nodeId, session, backFolder} _ = do
    useLoader {nodeId, session, backFolder} loadFolders $
      \folders -> folderViewMain {folders, nodeId, session, backFolder}

type FolderViewProps = 
  ( 
    nodeId :: Int
  , folders:: FTree
  , session :: Session
  , backFolder :: Boolean
  )

folderViewMain :: Record FolderViewProps -> R.Element
folderViewMain props = R.createElement folderViewMainCpt props []

folderViewMainCpt :: R.Component FolderViewProps
folderViewMainCpt = here.component "folderViewMainCpt" cpt where
  cpt {nodeId, session, backFolder, folders: (NTree (LNode {parent_id: parentId, nodeType}) (folders))} _ = do
    let sid = sessionId session 
    let foldersS = A.sortBy sortFolders folders
    let children = makeFolderElements foldersS sid
    let backHome = isBackHome nodeType
    let parent = makeParentFolder parentId sid backFolder backHome

    pure $ H.div {className: "folders"} $ parent <> children

  makeFolderElements :: Array (NTree LNode) -> GT.SessionId -> Array R.Element
  makeFolderElements foldersS sid = makeFolderElementsMap <$> foldersS where
    makeFolderElementsMap :: NTree LNode -> R.Element
    makeFolderElementsMap (NTree (LNode node) _) = folder {style: FolderChild, text: node.name, nodeId: node.id, nodeType: node.nodeType, sid: sid} []

  makeParentFolder :: Maybe Int -> GT.SessionId -> Boolean -> Boolean -> Array R.Element
  makeParentFolder (Just parentId) sid _ _ =
    -- FIXME: The NodeType here should not be hardcoded to FolderPrivate but we currently can't get the actual NodeType
    -- without performing another API call. Also parentId is never being returned by this API even when it clearly exists
    [ folder {style: FolderUp, text: "..", nodeId: parentId, nodeType: GT.FolderPrivate, sid: sid} [] ]
  makeParentFolder Nothing _ _ true = [ H.a {className: "btn btn-primary", href: appPath Home} [ H.i { className: "fa fa-folder-open" } []
                                                                   , H.br {}
                                                                   , H.text ".."] ]
  makeParentFolder Nothing _ true _ = [ H.button {className: "btn btn-primary", on: { click: back } }  [ H.i { className: "fa fa-folder-open" } []
                                                                   , H.br {}
                                                                   , H.text ".."] ]
  makeParentFolder Nothing _ _ _ = []

  sortFolders :: FTree -> FTree -> Ordering
  sortFolders a b = compare (fTreeID a) (fTreeID b)

  isBackHome :: GT.NodeType -> Boolean
  isBackHome GT.FolderPrivate = true
  isBackHome GT.FolderPublic = true
  isBackHome GT.FolderShared = true
  isBackHome _ = false


type FolderProps = 
  (
    style :: FolderStyle
  , text :: String
  , nodeType :: GT.NodeType
  , nodeId :: Int
  , sid :: GT.SessionId
  )

folder :: R2.Component FolderProps
folder = R.createElement folderCpt

folderCpt :: R.Component FolderProps
folderCpt = here.component "folderCpt" cpt where
  cpt {style, text, nodeId, sid, nodeType} _ = do
    pure $ H.a {className: "btn btn-primary", href: "/#/" <> getFolderPath nodeType sid nodeId}  [ H.i { className: icon style nodeType } []
                                                                   , H.br {}
                                                                   , H.text text]
  
  icon :: FolderStyle -> GT.NodeType -> String
  icon FolderUp _ = "fa fa-folder-open"
  icon _ nodeType = GT.fldr nodeType false

  getFolderPath :: GT.NodeType -> GT.SessionId -> Int -> String
  getFolderPath nodeType sid nodeId = appPath $ fromMaybe Home $ nodeTypeAppRoute nodeType sid nodeId

backButton :: R.Element
backButton = 
  H.button {
    className: "btn btn-primary"
  , on: {click: back}
  } [
    H.i { className: "fa fa-arrow-left", title: "Previous view"} []
  ]

homeButton :: R.Element
homeButton =
  H.a {
    className: "btn btn-primary"
  , href: appPath Home
  } [
    H.i { className: "fa fa-home", title: "Back to home"} []
  ]
  

loadFolders :: Record Props -> Aff FTree
loadFolders {nodeId, session} = get session $ TreeFirstLevel (Just nodeId) ""
