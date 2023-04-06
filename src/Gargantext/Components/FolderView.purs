module Gargantext.Components.FolderView where

import Gargantext.Prelude

import DOM.Simple (window)
import Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse_)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Gargantext.AsyncTasks as GAT
import Gargantext.Components.App.Store (Boxes)
import Gargantext.Components.App.Store as AppStore
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.BaseModal (hideModal)
import Gargantext.Components.Bootstrap.Types (Elevation(..), Variant(..))
import Gargantext.Components.Forest.Tree.Node.Action.Add (AddNodeValue(..), addNode)
import Gargantext.Components.Forest.Tree.Node.Action.Contact as Contact
import Gargantext.Components.Forest.Tree.Node.Action.Delete (deleteNode, unpublishNode)
import Gargantext.Components.Forest.Tree.Node.Action.Link (linkNodeReq)
import Gargantext.Components.Forest.Tree.Node.Action.Merge (mergeNodeReq)
import Gargantext.Components.Forest.Tree.Node.Action.Move (moveNodeReq)
import Gargantext.Components.Forest.Tree.Node.Action.Rename (RenameValue(..), rename)
import Gargantext.Components.Forest.Tree.Node.Action.Share as Share
import Gargantext.Components.Forest.Tree.Node.Action.Types (Action(..))
import Gargantext.Components.Forest.Tree.Node.Action.Update (updateRequest)
import Gargantext.Components.Forest.Tree.Node.Action.Upload (uploadArbitraryFile, uploadFile)
import Gargantext.Components.Forest.Tree.Node.Box (nodePopupView)
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree.Types (SubTreeOut(..))
import Gargantext.Components.GraphQL.Endpoints (getNode, getTreeFirstLevel)
import Gargantext.Components.GraphQL.Node (Node)
import Gargantext.Components.GraphQL.Tree (TreeFirstLevel, TreeNode)
import Gargantext.Config.REST (AffRESTError, logRESTError)
import Gargantext.Config.Utils (handleRESTError)
import Gargantext.Hooks.LinkHandler (useLinkHandler)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (AppRoute(Home), appPath, nodeTypeAppRoute)
import Gargantext.Sessions (Session(..), sessionId)
import Gargantext.Types (NodeType(..), SessionId)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.FolderView"

type Props =
  ( nodeId     :: Int
  , session    :: Session
  )

data FolderStyle = FolderUp | FolderChild

folderView :: R2.Leaf Props
folderView = R2.leaf folderViewCpt
folderViewCpt :: R.Component Props
folderViewCpt = here.component "folderViewCpt" cpt where
  cpt { nodeId, session } _ = do
    reload <- T.useBox T2.newReload
    reload' <- T.useLive T.unequal reload
    useLoader { errorHandler
              , loader: loadFolders
              , path: { nodeId, session, reload: reload'}
              , render: \folders -> folderViewMain { folders
                                                   , nodeId
                                                   , reload
                                                   , session
                                                   } [] }
    where
      errorHandler = logRESTError here "[folderView]"

type FolderViewProps =
  ( folders       :: TreeFirstLevel
  , nodeId        :: Int
  , reload        :: T.Box T2.Reload
  , session       :: Session
  )

folderViewMain :: R2.Component FolderViewProps
folderViewMain = R.createElement folderViewMainCpt
folderViewMainCpt :: R.Component FolderViewProps
folderViewMainCpt = here.component "folderViewMainCpt" cpt where
  cpt props@{ folders: {parent: parentNode, children, root} } _ = do
    let folders' = A.sortBy sortFolders children
    let parent = makeParentFolder root parentNode props
    let childrenEl = makeFolderElements folders' props

    pure $

      H.div
      { className: "folder-view" } $
      parent <> childrenEl

  makeFolderElements :: Array TreeNode -> Record FolderViewProps -> Array R.Element
  makeFolderElements folders' props = makeFolderElementsMap <$> folders' where
    makeFolderElementsMap :: TreeNode -> R.Element
    makeFolderElementsMap node = folder { nodeId: node.id
                                        , linkId: node.id
                                        , nodeType: node.node_type
                                        , linkNodeType: node.node_type
                                        , parentId: props.nodeId
                                        , reload: props.reload
                                        , session: props.session
                                        , style: FolderChild
                                        , text: node.name
                                        , disabled: false
                                        }

  makeParentFolder :: TreeNode -> Maybe TreeNode -> Record FolderViewProps -> Array R.Element
  makeParentFolder root (Just parent) props =
    [
      folder
      { disabled: disabled parent
      , linkId: parent.id
      , linkNodeType: parent.node_type
      , nodeId: root.id
      , nodeType: root.node_type
      , parentId: parent.id
      , reload: props.reload
      , session: props.session
      , style: FolderUp
      , text: "..."
      }
    ]
    where
      disabled { node_type } = if node_type == GT.FolderShared then true else false
  makeParentFolder _ Nothing _ = []

  sortFolders :: TreeNode-> TreeNode -> Ordering
  sortFolders a b = compare a.id b.id

type FolderProps =
  ( disabled      :: Boolean
  , linkNodeType  :: GT.NodeType
  , linkId        :: Int
  , nodeType      :: GT.NodeType
  , nodeId        :: Int
  , parentId      :: Int
  , reload        :: T.Box T2.Reload
  , session       :: Session
  , style         :: FolderStyle
  , text          :: String
  )

folder :: R2.Leaf FolderProps
folder = R2.leaf folderCpt
folderCpt :: R.Component FolderProps
folderCpt = here.component "folderCpt" cpt where
  cpt props@{ disabled
            , linkId
            , linkNodeType
            , nodeId
            , nodeType
            , parentId
            , reload
            , session
            , style
            , text
            } _ = do
    -- | States
    -- |

    boxes <- AppStore.use
    isBoxVisible <- T.useBox false

    -- | Computed
    -- |

    let sid = sessionId session
    let rootId = treeId session
    let dispatch a = performAction a { boxes, nodeId, parentId, reload, session, isBoxVisible }

    -- | Render
    -- |
    pure $

      H.div
      { className: "folder-view-item" }
      [
        H.a
        { className: "folder-view-item__body"
        , href: "/#/" <> href linkId rootId linkNodeType sid
        }
        [
          B.ripple
          { variant: Secondary }
          [
            B.icon
            { className: "folder-view-item__icon"
            , name: icon style nodeType
            }
          ,
            B.div'
            { className: "folder-view-item__text" }
            text
          ]
        ]
      ,
        H.div
        { className: "folder-view-item__settings" }
        [
          B.iconButton
          { name: "flower-7"
          , callback: \_ -> T.write_ true isBoxVisible
          , title:
                "Each node of the Tree can perform some actions.\n"
              <> "Click here to execute one of them."
          , variant: Secondary
          , elevation: Level1
          , overlay: true
          , focusRing: false
          }
        ]
      ,
        -- // Modals //
        B.baseModal
        { isVisibleBox: isBoxVisible
        , noBody: true
        , noHeader: true
        , modalClassName: "forest-tree-node-modal"
        }
        [
          nodePopupView
          { boxes
          , closeCallback: \_ -> T.write_ false isBoxVisible
          , dispatch: dispatch
          , id: props.nodeId
          , nodeType: props.nodeType
          , name: props.text
          , session
          }
        ]
      ]

  href :: Int -> Int -> NodeType -> SessionId -> String
  href lId rootId nType sId
    | rootId == lId  = appPath Home
    | otherwise      = appPath $ getFolderPath nType sId lId

  icon :: FolderStyle -> GT.NodeType -> String
  icon FolderUp _  = "folder-open"
  icon _        nt = GT.getIcon nt true

  getFolderPath :: GT.NodeType -> GT.SessionId -> Int -> AppRoute
  getFolderPath nodeType sid nodeId = fromMaybe Home $ nodeTypeAppRoute nodeType sid nodeId

backButton :: R2.Component ()
backButton = R.createElement backButtonCpt
backButtonCpt :: R.Component ()
backButtonCpt = here.component "backButton" cpt where
  cpt _ _ = do
    { goToPreviousPage } <- useLinkHandler

    pure $
      H.button {
        className: "btn btn-primary"
      , on: { click: \_ -> goToPreviousPage unit }
      } [
        H.i { className: "fa fa-arrow-left", title: "Previous view"} []
      ]

backButtonSmart :: R2.Component (nodeId :: Int, session :: Session)
backButtonSmart = R.createElement backButtonSmartCpt

backButtonSmartCpt :: R.Component (nodeId :: Int, session :: Session)
backButtonSmartCpt = here.component "backButtonSmart" cpt where
  cpt {nodeId, session} _ = do
    reload <- T.useBox T2.newReload
    reload' <- T.useLive T.unequal reload
    useLoader { errorHandler
              , loader: loadNode
              , path: { nodeId, session, reload: reload' }
              , render: \node -> backButtonSmartMain { node, session } []
    }
    where
      errorHandler = logRESTError here "[folderView]"

backButtonSmartMain :: R2.Component (node :: Node, session :: Session)
backButtonSmartMain = R.createElement backButtonSmartMainCpt

backButtonSmartMainCpt :: R.Component (node :: Node, session :: Session)
backButtonSmartMainCpt = here.component "backButtonSmartMain" cpt where
  cpt { node, session } _ = do
    handlers <- useLinkHandler
    let rootId = treeId session

    pure $
      H.button {
        className: "btn btn-primary"
      , on: { click: action rootId node.parent_id handlers }
      } [
        H.i { className: "fa fa-arrow-left", title: "Previous view"} []
      ]
    where
      action rootId pId handlers
        | rootId == pId = handlers.goToRoute Home
        | otherwise = handlers.goToPreviousPage unit

treeId :: Session -> Int
treeId (Session {treeId: tId}) = tId

type LoadProps =
  (
    session :: Session,
    nodeId :: Int,
    reload :: T2.Reload
  )

loadFolders :: Record LoadProps -> AffRESTError TreeFirstLevel
loadFolders {nodeId, session} = getTreeFirstLevel session nodeId

loadNode :: Record LoadProps -> AffRESTError Node
loadNode {nodeId, session} = getNode session nodeId

type PerformActionProps =
  ( boxes         :: Boxes
  , nodeId        :: Int
  , parentId      :: Int
  , reload        :: T.Box T2.Reload
  , session       :: Session
  , isBoxVisible  :: T.Box Boolean
  )

performAction :: Action -> Record PerformActionProps -> Aff Unit
performAction = performAction' where
  performAction' (DeleteNode nt) p = deleteNode' nt p
  performAction' (DoSearch task) p = doSearch task p
  performAction' (UpdateNode params) p = updateNode params p
  performAction' (RenameNode name) p = renameNode name p
  performAction' (ShareTeam username) p = shareTeam username p
  performAction' (SharePublic { params }) p = sharePublic params p
  performAction' (AddContact params) p = addContact params p
  performAction' (AddNode name nodeType) p = addNode' name nodeType p
  performAction' (UploadFile nodeType fileType fileFormat lang mName contents selection) p =
    uploadFile' nodeType fileType fileFormat lang mName contents p selection
  performAction' (UploadArbitraryFile fileFormat mName blob selection) p =
    uploadArbitraryFile' fileFormat mName blob p selection
  performAction' DownloadNode _ = liftEffect $ here.log "[performAction] DownloadNode"
  performAction' (MoveNode {params}) p = moveNode params p
  performAction' (MergeNode {params}) p = mergeNode params p
  performAction' (LinkNode { nodeType, params }) p = linkNode nodeType params p
  performAction' NoAction _ = liftEffect $ here.log "[performAction] NoAction"
  performAction' CloseBox p = closeBox p
  performAction' _ _ = liftEffect $ here.log "[performAction] unsupported action"

  closeBox { isBoxVisible, nodeId } =
    liftEffect $ do
      T.write_ false isBoxVisible
      -- @XXX ReactJS unreactive ref
      --
      -- /!\ extra care here:
      --
      --  - due to a ReactJS yet another flaw, we have to make an extra closing
      --    modal method call here (bc. even if the `T.Box` change its value
      --    no reactivity will be perfomed, for some unknown reason, and
      --    the modal would so partially close)
      --
      --  - also make an extra assumption here, as the `querySelector` used for
      --    modal close call should be the same as the selector qualifying the
      --    created <base-modal>)
      hideModal window $ "#" <> (show nodeId)

  refreshFolders p@{ boxes: { reloadForest }, reload } = do
    closeBox p
    liftEffect $ T2.reload reload
    liftEffect $ T2.reload reloadForest

  deleteNode' nt p@{ nodeId: id, parentId: parent_id, session } = do
    case nt of
      NodePublic FolderPublic  -> void $ deleteNode session id
      NodePublic _             -> void $ unpublishNode session (Just parent_id) id
      _                        -> void $ deleteNode session id
    refreshFolders p

  doSearch task { boxes: { tasks }, nodeId: id } = liftEffect $ do
    GAT.insert id task tasks
    here.log2 "[performAction] DoSearch task:" task

  updateNode params { boxes: { errors, tasks }, nodeId: id, session } = do
    eTask <- updateRequest params session id
    handleRESTError errors eTask $ \task -> liftEffect $ do
      GAT.insert id task tasks
      here.log2 "[performAction] UpdateNode task:" task

  shareTeam username { boxes: { errors }, nodeId: id, session } = do
    eTask <- Share.shareReq session id $ Share.ShareTeamParams { username }
    handleRESTError errors eTask $ \_task -> pure unit

  sharePublic params p@{ boxes: { errors }, session } = traverse_ f params where
    f (SubTreeOut { in: inId, out }) = do
      eTask <- Share.shareReq session inId $ Share.SharePublicParams { node_id: out }
      handleRESTError errors eTask $ \_task -> pure unit
      refreshFolders p

  addContact params { nodeId: id, session } =
    void $ Contact.contactReq session id params

  uploadFile' nodeType fileType fileFormat lang mName contents { boxes: { errors, tasks }, nodeId: id, session } selection = do
    eTask <- uploadFile { contents, fileType, fileFormat, lang, id, nodeType, mName, selection, session }
    handleRESTError errors eTask $ \task -> liftEffect $ do
      GAT.insert id task tasks
      here.log2 "[performAction] UploadFile, uploaded, task:" task

  uploadArbitraryFile' fileFormat mName blob { boxes: { errors, tasks }, nodeId: id, session } selection = do
    eTask <- uploadArbitraryFile session id { blob, fileFormat, mName } selection
    handleRESTError errors eTask $ \task -> liftEffect $ do
      GAT.insert id task tasks
      here.log2 "[performAction] UploadArbitraryFile, uploaded, task:" task

  moveNode params p@{ boxes: { errors }, session } = traverse_ f params where
    f (SubTreeOut { in: in', out }) = do
      eTask <- moveNodeReq session in' out
      handleRESTError errors eTask $ \_task -> pure unit
      refreshFolders p

  mergeNode params p@{ boxes: { errors }, session } = traverse_ f params where
    f (SubTreeOut { in: in', out }) = do
      eTask <- mergeNodeReq session in' out
      handleRESTError errors eTask $ \_task -> pure unit
      refreshFolders p

  linkNode nodeType params p@{ boxes: { errors }, session } = traverse_ f params where
    f (SubTreeOut { in: in', out }) = do
      eTask <- linkNodeReq session nodeType in' out
      handleRESTError errors eTask $ \_task -> pure unit
      refreshFolders p

  renameNode name p@{ boxes: { errors }, nodeId: id, session } = do
    eTask <- rename session id $ RenameValue { text: name }
    handleRESTError errors eTask $ \_task -> pure unit
    refreshFolders p

  addNode' name nodeType p@{ boxes: { errors }, nodeId: id, session } = do
    eTask <- addNode session id $ AddNodeValue {name, nodeType}
    handleRESTError errors eTask $ \_task -> pure unit
    refreshFolders p
