module Gargantext.Components.Forest.Tree where

import Gargantext.Prelude

import Data.Array as A
import Data.Array as Array
import Data.Maybe (Maybe(..), isJust)
import Data.Traversable (intercalate, traverse, traverse_)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Gargantext.AsyncTasks as GAT
import Gargantext.Components.App.Store (Boxes)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Forest.Tree.Node (blankNodeSpan, nodeSpan)
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
import Gargantext.Components.Forest.Tree.Node.Action.Upload (uploadFile, uploadArbitraryFile, uploadFrameCalc)
import Gargantext.Components.Forest.Tree.Node.Action.WriteNodesDocuments (documentsFromWriteNodesReq)
import Gargantext.Components.Forest.Tree.Node.Tools.FTree (FTree, LNode(..), NTree(..), fTreeID)
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree.Types (SubTreeOut(..))
import Gargantext.Config.REST (AffRESTError, logRESTError)
import Gargantext.Config.Utils (handleRESTError)
import Gargantext.Ends (Frontends)
import Gargantext.Hooks.Loader (useLoaderEffect)
import Gargantext.Hooks.Session (useSession)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, get, mkNodeId)
import Gargantext.Sessions.Types (useOpenNodesMemberBox, openNodesInsert, openNodesDelete)
import Gargantext.Types (Handed, ID, isPublic, publicize)
import Gargantext.Types as GT
import Gargantext.Utils ((?))
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Record.Extra as RecordE
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree"

-- Shared by every component here
type Common =
  ( boxes     :: Boxes
  , frontends :: Frontends
  , handed    :: Handed
  , reload    :: T2.ReloadS
  )

type LoaderProps =
 ( root    :: ID
 , session :: Session
 | Common )

type NodeProps =
 ( reloadTree :: T2.ReloadS
 | Common )

type TreeProps =
 ( root :: ID
 , session :: Session
 , tree :: FTree
 | NodeProps )

type ChildrenTreeProps =
  ( childProps :: { children'  :: Array FTree
                  , folderOpen :: T.Box Boolean
                  , render     :: R2.Leaf TreeProps }
  | TreeProps )

--- The properties tree shares in common with performAction
type PACommon =
  ( boxes      :: Boxes
  , reloadTree :: T2.ReloadS
  , tree       :: FTree
  )

-- The properties tree shares in common with nodeSpan
type NSCommon =
  ( frontends :: Frontends
  , handed    :: Handed  )

-- The annoying 'render' here is busting a cycle in the low tech
-- way. This function is only called by functions in this module, so
-- we just have to careful in what we pass.
type ChildLoaderProps =
  ( id     :: ID
  , render :: R2.Leaf TreeProps
  , root   :: ID
  , session :: Session
  | NodeProps
  )

type PerformActionProps =
  ( isBoxVisible :: T.Box Boolean
  , session      :: Session
  | PACommon
  )

-- | Loads and renders the tree starting at the given root node id.
treeLoader :: R2.Leaf ( key :: String | LoaderProps )
treeLoader = R2.leaf treeLoaderCpt
treeLoaderCpt :: R.Component ( key :: String | LoaderProps )
treeLoaderCpt = here.component "treeLoader" cpt where
-- treeLoaderCpt :: R.Memo LoaderProps
-- treeLoaderCpt = R.memo (here.component "treeLoader" cpt) memoCmp where
--   memoCmp ({ root: t1 }) ({ root: t2 }) = t1 == t2
  cpt p@{ root, session } _ = do
    -- States
    -- app     <- T.useLive T.unequal p.reloadRoot
    state /\ stateBox <- R2.useBox' Nothing
    let fetch { root: r } = getNodeTree session r

    -- Hooks
    useLoaderEffect
      { errorHandler
      , loader: fetch
      , path: { root }
      , state: stateBox
      }

    -- Render
    pure $

      B.cloak
      { isDisplayed: isJust state
      , sustainingPhaseDuration: Just 50
      , cloakSlot:
          blankTree {}
      , defaultSlot:
          R2.fromMaybe state $ loaded
      }
      where
        loaded tree' = tree props where
          props = Record.merge common extra where
            common = RecordE.pick p :: Record Common
            extra = { reloadTree: p.reload, root, session, tree: tree' }
        errorHandler = logRESTError here "[treeLoader]"

getNodeTree :: Session -> ID -> AffRESTError FTree
getNodeTree session nodeId = get session $ GR.NodeAPI GT.Tree (Just nodeId) ""

getNodeTreeFirstLevel :: Session -> ID -> AffRESTError FTree
getNodeTreeFirstLevel session nodeId = get session $ GR.TreeFirstLevel (Just nodeId) ""

tree :: R2.Leaf TreeProps
tree props = R.createElement treeCpt props []
treeCpt :: R.Component TreeProps
treeCpt = here.component "tree" cpt where
  cpt p@{ boxes: boxes@{ forestOpen }
        , frontends
        , reload
        , root
        , session
        , tree: NTree (LNode { id, name, nodeType }) children } _ = do
    let nodeId = mkNodeId session id

    isBoxVisible  <- T.useBox false
    folderOpen    <- useOpenNodesMemberBox nodeId forestOpen
    folderOpen'   <- T.useLive T.unequal folderOpen

    pure $

      H.div
      { className: intercalate " "
          [ "maintree"
          , Array.null children' ?
              "maintree--no-child" $
              "maintree--with-child"
          ]
      }
      [
        H.div
        { className: "maintree__node" }
        [
          nodeSpan
          { boxes
          , dispatch: dispatch' isBoxVisible session
          , folderOpen
          , frontends
          , id
          , isLeaf
          , name
          , nodeType
          , reload
          , root
          , isBoxVisible
          , session
          }
        <>
          R2.when (folderOpen')
          (
            renderTreeChildren $
            { childProps:
                { children'
                , folderOpen
                , render: tree
                }
            } `Record.merge` p
          )
        ]
      ]
    where
      isLeaf = A.null children
      children' = A.sortWith fTreeID pubChildren
      pubChildren = if isPublic nodeType then map (map pub) children else children
      dispatch' isBoxVisible session a = performAction a (Record.merge common' extra) where
        common' = RecordE.pick p :: Record PACommon
        extra = { isBoxVisible, session }
  pub (LNode n@{ nodeType: t }) = LNode (n { nodeType = publicize t })



blankTree :: R2.Leaf ()
blankTree = R2.leaf blankTreeCpt
blankTreeCpt :: R.Component ()
blankTreeCpt = here.component "__blank__" cpt where
  cpt _ _ = pure $

    H.div
    { className: "maintree maintree--blank" }
    [
      H.div
      { className: "maintree__node" }
      [
        blankNodeSpan
        {}
      ]
    ]


renderTreeChildren :: R2.Leaf ChildrenTreeProps
renderTreeChildren = R2.leaf renderTreeChildrenCpt
renderTreeChildrenCpt :: R.Component ChildrenTreeProps
renderTreeChildrenCpt = here.component "renderTreeChildren" cpt where
  cpt p@{ childProps: { children'
                      , render }
        , root
        , session } _ = do
    pure $ R.fragment (map renderChild children')

    where
      nodeProps = RecordE.pick p :: Record NodeProps
      renderChild (NTree (LNode {id: cId}) _) = childLoader props [] where
        props = Record.merge nodeProps { id: cId, render, root, session }


childLoader :: R2.Component ChildLoaderProps
childLoader = R.createElement childLoaderCpt
childLoaderCpt :: R.Component ChildLoaderProps
childLoaderCpt = here.component "childLoader" cpt where
  cpt p@{ boxes: { reloadRoot }
        , reloadTree
        , render
        , root
        , session } _ = do
    let fetch _ = getNodeTreeFirstLevel session p.id

    -- States
    reload <- T.useBox T2.newReload
    state /\ stateBox <- R2.useBox' Nothing
    let reloads = [ reload, reloadRoot, reloadTree ]
    cache <- (A.cons p.id) <$> traverse (T.useLive T.unequal) reloads

    -- Hooks
    useLoaderEffect
      { errorHandler
      , loader: fetch
      , path: cache
      , state: stateBox
      }

    -- Render
    pure $

      B.cloak
      { isDisplayed: isJust state
      , sustainingPhaseDuration: Just 50
      , cloakSlot:
          blankTree {}
      , defaultSlot:
          R2.fromMaybe state $ paint reload
      }

    where
      errorHandler = logRESTError here "[childLoader]"
      paint reload tree' = render (Record.merge base extra) where
        base = nodeProps { reload = reload }
        extra = { root, session, tree: tree' }
        nodeProps = RecordE.pick p :: Record NodeProps

closeBox { isBoxVisible } =
  liftEffect $ T.write_ false isBoxVisible

refreshTree p@{ reloadTree } = liftEffect $ closeBox p *> T2.reload reloadTree

deleteNode' nt p@{ boxes: { forestOpen }, session, tree: (NTree (LNode {id, parent_id}) _) } = do
  case nt of
    GT.NodePublic GT.FolderPublic -> void $ deleteNode session id
    GT.NodePublic _               -> void $ unpublishNode session parent_id id
    _                             -> void $ deleteNode session id
  liftEffect $ T.modify_ (openNodesDelete (mkNodeId session id)) forestOpen
  refreshTree p

doSearch task { boxes: { tasks }, tree: NTree (LNode {id}) _ } = liftEffect $ do
  GAT.insert id task tasks
  here.log2 "[doSearch] DoSearch task:" task

updateNode params p@{ boxes: { errors, tasks }, session, tree: (NTree (LNode {id}) _) } = do
  eTask <- updateRequest params session id
  handleRESTError errors eTask $ \task -> liftEffect $ do
    GAT.insert id task tasks
    here.log2 "[updateNode] UpdateNode task:" task
    closeBox p

renameNode name p@{ boxes: { errors }, session, tree: (NTree (LNode {id}) _) } = do
  eTask <- rename session id $ RenameValue { text: name }
  handleRESTError errors eTask $ \_task -> pure unit
  refreshTree p

shareTeam username { boxes: { errors }, session, tree: (NTree (LNode {id}) _)} = do
  eTask <- Share.shareReq session id $ Share.ShareTeamParams { username }
  handleRESTError errors eTask $ \_task -> pure unit

sharePublic params p@{ boxes: { errors, forestOpen }, session } = traverse_ f params where
  f (SubTreeOut { in: inId, out }) = do
    eTask <- Share.shareReq session inId $ Share.SharePublicParams { node_id: out }
    handleRESTError errors eTask $ \_task -> do
      liftEffect $ T.modify_ (openNodesInsert (mkNodeId p.session out)) forestOpen
      refreshTree p

addContact params { boxes: { errors }, session, tree: (NTree (LNode {id}) _) } = do
  eTask <- Contact.contactReq session id params
  handleRESTError errors eTask $ \_task -> pure unit

addNode' name nodeType p@{ boxes: { errors, forestOpen }, session, tree: (NTree (LNode { id }) _) } = do
  eId <- addNode session id $ AddNodeValue { name, nodeType }
  handleRESTError errors eId $ \_id -> liftEffect $ do
    liftEffect $ T.modify_ (openNodesInsert (mkNodeId session id)) forestOpen
    refreshTree p

uploadFile' nodeType fileType fileFormat lang mName contents p@{ boxes: { errors, tasks }, session, tree: (NTree (LNode { id }) _) } selection = do
  eTask <- uploadFile { contents, fileFormat, fileType, id, lang, mName, nodeType, selection, session }
  handleRESTError errors eTask $ \task -> liftEffect $ do
    GAT.insert id task tasks
    here.log2 "[uploadFile'] UploadFile, uploaded, task:" task
    closeBox p

uploadArbitraryFile' fileFormat mName blob p@{ boxes: { errors, tasks }, session, tree: (NTree (LNode { id }) _) } selection = do
  eTask <- uploadArbitraryFile session id { blob, fileFormat, mName } selection
  handleRESTError errors eTask $ \task -> liftEffect $ do
    GAT.insert id task tasks
    here.log2 "[uploadArbitraryFile'] UploadArbitraryFile, uploaded, task:" task

uploadFrameCalc' lang p@{ boxes: { errors, tasks }, session, tree: (NTree (LNode { id }) _) } selection = do
  eTask <- uploadFrameCalc session id lang selection
  handleRESTError errors eTask $ \task -> liftEffect $ do
    GAT.insert id task tasks
    here.log2 "[performAction] UploadFrameCalc, uploaded, task:" task

moveNode params p@{ boxes: { errors, forestOpen }, session } = traverse_ f params where
  f (SubTreeOut { in: in', out }) = do
    eTask <- moveNodeReq session in' out
    handleRESTError errors eTask $ \_task -> pure unit
    liftEffect $ T.modify_ (openNodesInsert (mkNodeId session out)) forestOpen
    refreshTree p

mergeNode params p@{ boxes: { errors }, session } = traverse_ f params where
  f (SubTreeOut { in: in', out }) = do
    eTask <- mergeNodeReq session in' out
    handleRESTError errors eTask $ \_task -> pure unit
    refreshTree p

linkNode nodeType params p@{ boxes: { errors }, session } = traverse_ f params where
  f (SubTreeOut { in: in', out }) = do
    eTask <- linkNodeReq session nodeType in' out
    handleRESTError errors eTask $ \_task -> pure unit
    refreshTree p

documentsFromWriteNodes id p@{ boxes: { errors }, session } = do
  eTask <- documentsFromWriteNodesReq session id
  handleRESTError errors eTask $ \_task -> pure unit
  refreshTree p

-- | This thing is basically a hangover from when garg was a thermite
-- | application. we should slowly get rid of it.
performAction :: Action -> Record PerformActionProps -> Aff Unit
performAction (DeleteNode nt) p                               = deleteNode' nt p
performAction (DoSearch task) p                               = doSearch task p
performAction (UpdateNode params) p                           = updateNode params p
performAction (RenameNode name) p                             = renameNode name p
performAction (ShareTeam username) p                          = shareTeam username p
performAction (SharePublic { params }) p                      = sharePublic params p
performAction (AddContact params) p                           = addContact params p
performAction (AddNode name nodeType) p                       = addNode' name nodeType p
performAction (UploadFrameCalc lang selection) p              = uploadFrameCalc' lang p selection
performAction (UploadFile nodeType fileType fileFormat lang mName contents selection) p =
  uploadFile' nodeType fileType fileFormat lang mName contents p selection
performAction (UploadArbitraryFile fileFormat mName blob selection) p              =
  uploadArbitraryFile' fileFormat mName blob p selection
performAction DownloadNode _                                  = liftEffect $ here.log "[performAction] DownloadNode"
performAction (MoveNode {params}) p                           = moveNode params p
performAction (MergeNode {params}) p                          = mergeNode params p
performAction (LinkNode { nodeType, params }) p               = linkNode nodeType params p
performAction RefreshTree p                                   = refreshTree p
performAction CloseBox p                                      = closeBox p
performAction (DocumentsFromWriteNodes { id }) p              = documentsFromWriteNodes id p
performAction NoAction _                                      = liftEffect $ here.log "[performAction] NoAction"
