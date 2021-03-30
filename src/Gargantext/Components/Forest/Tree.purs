module Gargantext.Components.Forest.Tree where

import Gargantext.Prelude

import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Traversable (traverse_, traverse)
import Data.Tuple (snd)
import DOM.Simple.Console (log, log2)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Record.Extra as RecordE
import Toestand as T

import Gargantext.AsyncTasks as GAT
import Gargantext.Components.Forest.Tree.Node (nodeSpan)
import Gargantext.Components.Forest.Tree.Node.Action (Action(..))
import Gargantext.Components.Forest.Tree.Node.Action.Add (AddNodeValue(..), addNode)
import Gargantext.Components.Forest.Tree.Node.Action.Contact as Contact
import Gargantext.Components.Forest.Tree.Node.Action.Delete (deleteNode, unpublishNode)
import Gargantext.Components.Forest.Tree.Node.Action.Link (linkNodeReq)
import Gargantext.Components.Forest.Tree.Node.Action.Merge (mergeNodeReq)
import Gargantext.Components.Forest.Tree.Node.Action.Move (moveNodeReq)
import Gargantext.Components.Forest.Tree.Node.Action.Rename (RenameValue(..), rename)
import Gargantext.Components.Forest.Tree.Node.Action.Share as Share
import Gargantext.Components.Forest.Tree.Node.Action.Update (updateRequest)
import Gargantext.Components.Forest.Tree.Node.Action.Upload (uploadFile, uploadArbitraryFile)
import Gargantext.Components.Forest.Tree.Node.Tools.FTree (FTree, LNode(..), NTree(..), fTreeID)
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree.Types (SubTreeOut(..))
import Gargantext.Ends (Frontends)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (AppRoute)
import Gargantext.Routes as GR
import Gargantext.Sessions (OpenNodes, Session, mkNodeId, get)
import Gargantext.Types (Handed, ID, isPublic, publicize, switchHanded)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree"

-- Shared by every component here + performAction + nodeSpan
type Universal =
  ( reloadRoot :: T.Box T2.Reload )

-- Shared by every component here + nodeSpan
type Global =
  ( frontends  :: Frontends
  , handed     :: Handed
  , route      :: T.Box AppRoute
 , tasks       :: T.Box GAT.Storage
  | Universal )

-- Shared by every component here
type Common = (
   forestOpen :: T.Box OpenNodes
 , reload     :: T.Box T2.Reload
 | Global
 )

type LoaderProps = ( session :: Session, root :: ID | Common )

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
    useLoader { root } fetch loaded where
      loaded tree' = tree props where
        props = Record.merge common extra where
          common = RecordE.pick p :: Record Common
          extra = { tree: tree', reloadTree: p.reload, session }

getNodeTree :: Session -> ID -> Aff FTree
getNodeTree session nodeId = get session $ GR.NodeAPI GT.Tree (Just nodeId) ""

getNodeTreeFirstLevel :: Session -> ID -> Aff FTree
getNodeTreeFirstLevel session nodeId = get session $ GR.TreeFirstLevel (Just nodeId) ""

type NodeProps = ( reloadTree :: T.Box T2.Reload, session :: Session | Common )

type TreeProps = ( tree :: FTree | NodeProps )

tree :: R2.Leaf TreeProps
tree props = R.createElement treeCpt props []

treeCpt :: R.Component TreeProps
treeCpt = here.component "tree" cpt where
  cpt p@{ session, tree: NTree (LNode { id, name, nodeType }) children } _ = do
    setPopoverRef <- R.useRef Nothing
    folderOpen <- T2.useMemberBox nodeId p.forestOpen
    open <- T.useLive T.unequal folderOpen
    pure $ H.ul { className: ulClass }
      [ H.div { className: divClass } -- TODO: naughty div should not be in a ul
        [ H.li { className: childrenClass children }
          [ nodeSpan (nsprops { folderOpen, name, id, nodeType, setPopoverRef, isLeaf })
            (renderChildren open)
          ]
        ]
      ]
    where
      isLeaf = A.null children
      nodeId = mkNodeId session id
      ulClass  = switchHanded "ml" "mr" p.handed <> "-auto tree"
      divClass = switchHanded "left" "right" p.handed <> "handed"
      children' = A.sortWith fTreeID pubChildren
      pubChildren = if isPublic nodeType then map (map pub) children else children
      renderChildren false = []
      renderChildren true = map renderChild children' where
        renderChild (NTree (LNode {id: cId}) _) = childLoader props [] where
          props = Record.merge nodeProps { id: cId, render: tree }
          nodeProps = RecordE.pick p :: Record NodeProps
      nsprops extra = Record.merge common extra' where
        common = RecordE.pick p :: Record NSCommon
        extra' = Record.merge extra { dispatch } where
          dispatch a = performAction a (Record.merge common' spr) where
            common' = RecordE.pick p :: Record PACommon
            spr = { setPopoverRef: extra.setPopoverRef }
  pub (LNode n@{ nodeType: t }) = LNode (n { nodeType = publicize t })
  childrenClass [] = "no-children"
  childrenClass _ = "with-children"

--- The properties tree shares in common with performAction
type PACommon =
  ( forestOpen   :: T.Box OpenNodes
  , reloadTree   :: T.Box T2.Reload
  , session      :: Session
  , tasks        :: T.Box GAT.Storage
  , tree         :: FTree
  | Universal )

-- The properties tree shares in common with nodeSpan
type NSCommon = ( session :: Session | Global )

-- The annoying 'render' here is busting a cycle in the low tech
-- way. This function is only called by functions in this module, so
-- we just have to careful in what we pass.
type ChildLoaderProps = ( id :: ID, render :: R2.Leaf TreeProps | NodeProps )

childLoader :: R2.Component ChildLoaderProps
childLoader = R.createElement childLoaderCpt

childLoaderCpt :: R.Component ChildLoaderProps
childLoaderCpt = here.component "childLoader" cpt where
  cpt p@{ render } _ = do
    reload <- T.useBox T2.newReload
    let reloads = [ reload, p.reloadTree, p.reloadRoot ]
    cache <- (A.cons p.id) <$> traverse (T.useLive T.unequal) reloads
    useLoader cache fetch (paint reload)
    where
      fetch _ = getNodeTreeFirstLevel p.session p.id
      paint reload tree' = render (Record.merge base extra) where
        base = nodeProps { reload = reload }
        extra = { tree: tree' }
        nodeProps = RecordE.pick p :: Record NodeProps

type PerformActionProps =
  ( setPopoverRef :: R.Ref (Maybe (Boolean -> Effect Unit)) | PACommon )

-- | This thing is basically a hangover from when garg was a thermite
-- | application. we should slowly get rid of it.
performAction :: Action -> Record PerformActionProps -> Aff Unit
performAction (DeleteNode nt) p@{ forestOpen
                                , session
                                , tree: (NTree (LNode {id, parent_id}) _) } = do
  case nt of
    GT.NodePublic GT.FolderPublic -> void $ deleteNode session nt id
    GT.NodePublic _               -> void $ unpublishNode session parent_id id
    _                             -> void $ deleteNode session nt id
  liftEffect $ T.modify_ (Set.delete (mkNodeId session id)) forestOpen
  performAction RefreshTree p
performAction (DoSearch task) p@{ tasks
                                , tree: (NTree (LNode {id}) _) } = liftEffect $ do
  GAT.insert id task tasks
  log2 "[performAction] DoSearch task:" task
performAction (UpdateNode params) p@{ tasks
                                    , tree: (NTree (LNode {id}) _) } = do
  task <- updateRequest params p.session id
  liftEffect $ do
    GAT.insert id task tasks
    log2 "[performAction] UpdateNode task:" task
performAction (RenameNode name) p@{ tree: (NTree (LNode {id}) _) } = do
  void $ rename p.session id $ RenameValue { text: name }
  performAction RefreshTree p
performAction (ShareTeam username) p@{ tree: (NTree (LNode {id}) _)} =
  void $ Share.shareReq p.session id $ Share.ShareTeamParams {username}
performAction (SharePublic { params }) p@{ forestOpen } = traverse_ f params where
  f (SubTreeOut { in: inId, out }) = do
    void $ Share.shareReq p.session inId $ Share.SharePublicParams { node_id: out }
    liftEffect $ T.modify_ (Set.insert (mkNodeId p.session out)) forestOpen
    performAction RefreshTree p
performAction (AddContact params) p@{ tree: (NTree (LNode {id}) _) } =
    void $ Contact.contactReq p.session id params
performAction (AddNode name nodeType) p@{ forestOpen
                                        , tree: (NTree (LNode { id }) _) } = do
  task <- addNode p.session id $ AddNodeValue {name, nodeType}
  liftEffect $ T.modify_ (Set.insert (mkNodeId p.session id)) forestOpen
  performAction RefreshTree p
performAction (UploadFile nodeType fileType mName blob) p@{ tasks
                                                          , tree: (NTree (LNode { id }) _) } = do
  task <- uploadFile p.session nodeType id fileType {mName, blob}
  liftEffect $ do
    GAT.insert id task tasks
    log2 "[performAction] UploadFile, uploaded, task:" task
performAction (UploadArbitraryFile mName blob) p@{ tasks
                                                 , tree: (NTree (LNode { id }) _) } = do
  task <- uploadArbitraryFile p.session id { blob, mName }
  liftEffect $ do
    GAT.insert id task tasks
    log2 "[performAction] UploadArbitraryFile, uploaded, task:" task
performAction DownloadNode _ = liftEffect $ log "[performAction] DownloadNode"
performAction (MoveNode {params}) p@{ forestOpen
                                    , session } = traverse_ f params where
  f (SubTreeOut { in: in', out }) = do
    void $ moveNodeReq p.session in' out
    liftEffect $ T.modify_ (Set.insert (mkNodeId session out)) forestOpen
    performAction RefreshTree p
performAction (MergeNode { params }) p = traverse_ f params where
  f (SubTreeOut { in: in', out }) = do
    void $ mergeNodeReq p.session in' out
    performAction RefreshTree p
performAction (LinkNode { nodeType, params }) p = traverse_ f params where
  f (SubTreeOut { in: in', out }) = do
    void $ linkNodeReq p.session nodeType in' out
    performAction RefreshTree p
performAction RefreshTree p = do
  liftEffect $ T2.reload p.reloadTree
  performAction ClosePopover p
performAction NoAction _ = liftEffect $ log "[performAction] NoAction"
performAction ClosePopover { setPopoverRef } =
  liftEffect $ traverse_ (\set -> set false) (R.readRef setPopoverRef)
