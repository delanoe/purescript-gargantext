module Gargantext.Components.Forest.Tree where

import Gargantext.Prelude

import Data.Array as A
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_, traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Gargantext.AsyncTasks as GAT
import Gargantext.Components.App.Data (Boxes)
import Gargantext.Components.Forest.Tree.Node (nodeSpan)
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
import Gargantext.Components.Forest.Tree.Node.Tools.FTree (FTree, LNode(..), NTree(..), fTreeID)
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree.Types (SubTreeOut(..))
import Gargantext.Config.REST (RESTError)
import Gargantext.Config.Utils (handleRESTError)
import Gargantext.Ends (Frontends)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, get, mkNodeId)
import Gargantext.Sessions.Types (useOpenNodesMemberBox, openNodesInsert, openNodesDelete)
import Gargantext.Types (Handed, ID, isPublic, publicize, switchHanded)
import Gargantext.Types as GT
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
 , session :: Session
 | Common )

type TreeProps =
 ( root :: ID
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
  , session    :: Session
  , tree       :: FTree
  )

-- The properties tree shares in common with nodeSpan
type NSCommon =
  ( frontends :: Frontends
  , handed    :: Handed
  , session   :: Session  )

-- The annoying 'render' here is busting a cycle in the low tech
-- way. This function is only called by functions in this module, so
-- we just have to careful in what we pass.
type ChildLoaderProps =
  ( id     :: ID
  , render :: R2.Leaf TreeProps
  , root   :: ID
  | NodeProps )

type PerformActionProps =
  ( setPopoverRef :: R.Ref (Maybe (Boolean -> Effect Unit))
  | PACommon )

-- | Loads and renders the tree starting at the given root node id.
treeLoader :: R2.Component LoaderProps
treeLoader = R.createElement treeLoaderCpt
treeLoaderCpt :: R.Component LoaderProps
treeLoaderCpt = here.component "treeLoader" cpt where
-- treeLoaderCpt :: R.Memo LoaderProps
-- treeLoaderCpt = R.memo (here.component "treeLoader" cpt) memoCmp where
--   memoCmp ({ root: t1 }) ({ root: t2 }) = t1 == t2
  cpt p@{ root, session } _ = do
    -- app     <- T.useLive T.unequal p.reloadRoot
    let fetch { root: r } = getNodeTree session r
    useLoader { errorHandler
              , loader: fetch
              , path: { root }
              , render: loaded }
      where
        loaded tree' = tree props where
          props = Record.merge common extra where
            common = RecordE.pick p :: Record Common
            extra = { reloadTree: p.reload, root, session, tree: tree' }
        errorHandler err = here.log2 "[treeLoader] RESTError" err

getNodeTree :: Session -> ID -> Aff (Either RESTError FTree)
getNodeTree session nodeId = get session $ GR.NodeAPI GT.Tree (Just nodeId) ""

getNodeTreeFirstLevel :: Session -> ID -> Aff (Either RESTError FTree)
getNodeTreeFirstLevel session nodeId = get session $ GR.TreeFirstLevel (Just nodeId) ""

tree :: R2.Leaf TreeProps
tree props = R.createElement treeCpt props []
treeCpt :: R.Component TreeProps
treeCpt = here.component "tree" cpt where
  cpt p@{ boxes: boxes@{ forestOpen }
        , frontends
        , handed
        , reload
        , root
        , session
        , tree: NTree (LNode { id, name, nodeType }) children } _ = do
    setPopoverRef <- R.useRef Nothing
    folderOpen <- useOpenNodesMemberBox nodeId forestOpen
    pure $ H.ul { className: ulClass }
      [ H.li { className: childrenClass children' }
        [ nodeSpan { boxes
                   , dispatch: dispatch setPopoverRef
                   , folderOpen
                   , frontends
                   , id
                   , isLeaf
                   , name
                   , nodeType
                   , reload
                   , root
                   , session
                   , setPopoverRef }
          [ renderChildren (Record.merge p { childProps: { children', folderOpen, render: tree } } ) [] ]
        ]
      ]
    where
      isLeaf = A.null children
      nodeId = mkNodeId session id
      ulClass  = switchHanded "ml left" "mr right" handed <> "-auto tree handed"
      children' = A.sortWith fTreeID pubChildren
      pubChildren = if isPublic nodeType then map (map pub) children else children
      dispatch setPopoverRef a = performAction a (Record.merge common' spr) where
        common' = RecordE.pick p :: Record PACommon
        spr = { setPopoverRef }
  pub (LNode n@{ nodeType: t }) = LNode (n { nodeType = publicize t })
  childrenClass [] = "no-children"
  childrenClass _  = "with-children"


renderChildren :: R2.Component ChildrenTreeProps
renderChildren = R.createElement renderChildrenCpt
renderChildrenCpt :: R.Component ChildrenTreeProps
renderChildrenCpt = here.component "renderChildren" cpt where
  cpt p@{ childProps: { folderOpen } } _ = do
    folderOpen' <- T.useLive T.unequal folderOpen

    if folderOpen' then
      pure $ renderTreeChildren p []
    else
      pure $ H.div {} []

renderTreeChildren :: R2.Component ChildrenTreeProps
renderTreeChildren = R.createElement renderTreeChildrenCpt
renderTreeChildrenCpt :: R.Component ChildrenTreeProps
renderTreeChildrenCpt = here.component "renderTreeChildren" cpt where
  cpt p@{ childProps: { children'
                      , render }
        , root } _ = do
    pure $ R.fragment (map renderChild children')

    where
      nodeProps = RecordE.pick p :: Record NodeProps
      renderChild (NTree (LNode {id: cId}) _) = childLoader props [] where
        props = Record.merge nodeProps { id: cId, render, root }

childLoader :: R2.Component ChildLoaderProps
childLoader = R.createElement childLoaderCpt
childLoaderCpt :: R.Component ChildLoaderProps
childLoaderCpt = here.component "childLoader" cpt where
  cpt p@{ boxes: { reloadRoot }
        , reloadTree
        , render
        , root } _ = do
    reload <- T.useBox T2.newReload
    let reloads = [ reload, reloadRoot, reloadTree ]
    cache <- (A.cons p.id) <$> traverse (T.useLive T.unequal) reloads
    useLoader { errorHandler
              , loader: fetch
              , path: cache
              , render: paint reload }
    where
      errorHandler err = here.log2 "[childLoader] RESTError" err
      fetch _ = getNodeTreeFirstLevel p.session p.id
      paint reload tree' = render (Record.merge base extra) where
        base = nodeProps { reload = reload }
        extra = { root, tree: tree' }
        nodeProps = RecordE.pick p :: Record NodeProps

closePopover { setPopoverRef } =
   liftEffect $ traverse_ (\set -> set false) (R.readRef setPopoverRef)

refreshTree p@{ reloadTree } = liftEffect $ T2.reload reloadTree *> closePopover p

deleteNode' nt p@{ boxes: { forestOpen }, session, tree: (NTree (LNode {id, parent_id}) _) } = do
  case nt of
    GT.NodePublic GT.FolderPublic -> void $ deleteNode session nt id
    GT.NodePublic _               -> void $ unpublishNode session parent_id id
    _                             -> void $ deleteNode session nt id
  liftEffect $ T.modify_ (openNodesDelete (mkNodeId session id)) forestOpen
  refreshTree p

doSearch task p@{ boxes: { tasks }, tree: NTree (LNode {id}) _ } = liftEffect $ do
  GAT.insert id task tasks
  here.log2 "[doSearch] DoSearch task:" task

updateNode params { boxes: { errors, tasks }, session, tree: (NTree (LNode {id}) _) } = do
  eTask <- updateRequest params session id
  handleRESTError errors eTask $ \task -> liftEffect $ do
    GAT.insert id task tasks
    here.log2 "[updateNode] UpdateNode task:" task

renameNode name p@{ boxes: { errors }, session, tree: (NTree (LNode {id}) _) } = do
  eTask <- rename session id $ RenameValue { text: name }
  handleRESTError errors eTask $ \_task -> pure unit
  refreshTree p

shareTeam username p@{ boxes: { errors }, session, tree: (NTree (LNode {id}) _)} = do
  eTask <- Share.shareReq session id $ Share.ShareTeamParams { username }
  handleRESTError errors eTask $ \_task -> pure unit

sharePublic params p@{ boxes: { errors, forestOpen }, session } = traverse_ f params where
  f (SubTreeOut { in: inId, out }) = do
    eTask <- Share.shareReq session inId $ Share.SharePublicParams { node_id: out }
    handleRESTError errors eTask $ \_task -> do
      liftEffect $ T.modify_ (openNodesInsert (mkNodeId p.session out)) forestOpen
      refreshTree p

addContact params p@{ boxes: { errors }, session, tree: (NTree (LNode {id}) _) } = do
  eTask <- Contact.contactReq session id params
  handleRESTError errors eTask $ \_task -> pure unit

addNode' name nodeType p@{ boxes: { errors, forestOpen }, session, tree: (NTree (LNode { id }) _) } = do
  eId <- addNode session id $ AddNodeValue { name, nodeType }
  handleRESTError errors eId $ \_id -> liftEffect $ do
    liftEffect $ T.modify_ (openNodesInsert (mkNodeId session id)) forestOpen
    refreshTree p

uploadFile' nodeType fileType mName contents p@{ boxes: { errors, tasks }, session, tree: (NTree (LNode { id }) _) } selection = do
  eTask <- uploadFile { contents, fileType, id, mName, nodeType, selection, session }
  handleRESTError errors eTask $ \task -> liftEffect $ do
    GAT.insert id task tasks
    here.log2 "[uploadFile'] UploadFile, uploaded, task:" task

uploadArbitraryFile' mName blob p@{ boxes: { errors, tasks }, session, tree: (NTree (LNode { id }) _) } selection = do
  eTask <- uploadArbitraryFile session id { blob, mName } selection
  handleRESTError errors eTask $ \task -> liftEffect $ do
    GAT.insert id task tasks
    here.log2 "[uploadArbitraryFile'] UploadArbitraryFile, uploaded, task:" task

uploadFrameCalc' p@{ boxes: { errors, tasks }, session, tree: (NTree (LNode { id }) _) } = do
  eTask <- uploadFrameCalc session id
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
performAction UploadFrameCalc p                               = uploadFrameCalc' p
performAction (UploadFile nodeType fileType mName contents selection) p =
  uploadFile' nodeType fileType mName contents p selection
performAction (UploadArbitraryFile mName blob selection) p              =
  uploadArbitraryFile' mName blob p selection
performAction DownloadNode _                                  = liftEffect $ here.log "[performAction] DownloadNode"
performAction (MoveNode {params}) p                           = moveNode params p
performAction (MergeNode {params}) p                          = mergeNode params p
performAction (LinkNode { nodeType, params }) p               = linkNode nodeType params p
performAction RefreshTree p                                   = refreshTree p
performAction NoAction _                                      = liftEffect $ here.log "[performAction] NoAction"
performAction ClosePopover p                                  = closePopover p
