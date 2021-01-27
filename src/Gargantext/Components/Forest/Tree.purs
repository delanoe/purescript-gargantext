module Gargantext.Components.Forest.Tree where

import DOM.Simple.Console (log, log2)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Record.Extra as RecordE

import Gargantext.Prelude

import Gargantext.AsyncTasks as GAT
import Gargantext.Components.Forest.Tree.Node (nodeSpan)
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree.Types (SubTreeOut(..))
import Gargantext.Components.Forest.Tree.Node.Action (Action(..))
import Gargantext.Components.Forest.Tree.Node.Action.Add (AddNodeValue(..), addNode)
import Gargantext.Components.Forest.Tree.Node.Action.Delete (deleteNode, unpublishNode)
import Gargantext.Components.Forest.Tree.Node.Action.Move   (moveNodeReq)
import Gargantext.Components.Forest.Tree.Node.Action.Merge  (mergeNodeReq)
import Gargantext.Components.Forest.Tree.Node.Action.Link   (linkNodeReq)
import Gargantext.Components.Forest.Tree.Node.Action.Rename (RenameValue(..), rename)
import Gargantext.Components.Forest.Tree.Node.Action.Share   as Share
import Gargantext.Components.Forest.Tree.Node.Action.Contact as Contact
import Gargantext.Components.Forest.Tree.Node.Action.Update (updateRequest)
import Gargantext.Components.Forest.Tree.Node.Action.Upload (uploadFile, uploadArbitraryFile)
import Gargantext.Components.Forest.Tree.Node.Tools.FTree (FTree, LNode(..), NTree(..))
import Gargantext.Ends (Frontends, toUrl)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (AppRoute)
import Gargantext.Routes as GR
import Gargantext.Sessions (OpenNodes, Session, mkNodeId, get)
import Gargantext.Types (ID, isPublic, publicize)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Reload as GUR

thisModule :: String
thisModule = "Gargantext.Components.Forest.Tree"

------------------------------------------------------------------------
type CommonProps = (
    appReload     :: GUR.ReloadS
  , currentRoute  :: AppRoute
  , frontends     :: Frontends
  , handed        :: GT.Handed
  , openNodes     :: R.State OpenNodes
  , reload        :: GUR.ReloadS
  , session       :: Session
  )

------------------------------------------------------------------------
type Props = (
    asyncTasks :: GAT.Reductor
  , root       :: ID
  | CommonProps
  )

treeView :: R2.Component Props
treeView = R.createElement treeViewCpt

treeViewCpt :: R.Component Props
treeViewCpt = R.hooksComponentWithModule thisModule "treeView" cpt
  where
    cpt { appReload
        , asyncTasks
        , currentRoute
        , frontends
        , handed
        , openNodes
        , reload
        , root
        , session
        } _children = do
      pure $ treeLoadView { appReload
                          , asyncTasks
                          , currentRoute
                          , frontends
                          , handed
                          , openNodes
                          , reload
                          , root
                          , session
                          } []

treeLoadView :: R2.Component Props
treeLoadView = R.createElement treeLoadViewCpt

treeLoadViewCpt :: R.Component Props
treeLoadViewCpt = R.hooksComponentWithModule thisModule "treeLoadView" cpt
  where
    cpt { appReload
        , asyncTasks
        , currentRoute
        , frontends
        , handed
        , openNodes
        , reload
        , root
        , session
        } _children = do
      let fetch _ = getNodeTree session root
      let paint loaded = loadedTreeViewFirstLevel { appReload
                                                  , asyncTasks
                                                  , currentRoute
                                                  , frontends
                                                  , handed
                                                  , openNodes
                                                  , reload
                                                  , session
                                                  -- , tasks: tasksStruct root asyncTasks reload
                                                  , tree: loaded
                                                  } []
      useLoader { appCounter: GUR.value appReload
                , counter: GUR.value reload
                , root } fetch paint

--------------
getNodeTree :: Session -> GT.ID -> Aff FTree
getNodeTree session nodeId = get session $ GR.NodeAPI GT.Tree (Just nodeId) ""
--------------
getNodeTreeFirstLevel :: Session -> GT.ID -> Aff FTree
getNodeTreeFirstLevel session nodeId = get session $ GR.TreeFirstLevel (Just nodeId) ""
--------------
type TreeViewProps = (
    asyncTasks :: GAT.Reductor
  , tree       :: FTree
  | CommonProps
  )

loadedTreeViewFirstLevel :: R2.Component TreeViewProps
loadedTreeViewFirstLevel = R.createElement loadedTreeViewFirstLevelCpt

loadedTreeViewFirstLevelCpt :: R.Component TreeViewProps
loadedTreeViewFirstLevelCpt = R.hooksComponentWithModule thisModule "loadedTreeViewFirstLevel" cpt
  where
    cpt { appReload
        , asyncTasks
        , currentRoute
        , frontends
        , handed
        , openNodes
        , reload
        , session
        -- , tasks
        , tree
      } _ = do
      pure $ H.ul { className: "tree " <> if handed == GT.RightHanded then "mr-auto" else "ml-auto" } [
        H.div { className: if handed == GT.RightHanded then "righthanded" else "lefthanded" } [
          toHtmlFirstLevel { appReload
                           , asyncTasks
                           , currentRoute
                           , frontends
                           , handed
                           , openNodes
                           , reload
                           , reloadTree: reload
                           , session
                             -- , tasks
                           , tree
                           } []
            ]
        ]

------------------------------------------------------------------------


type ToHtmlProps = (
    asyncTasks :: GAT.Reductor
  , reloadTree :: GUR.ReloadS
  -- , tasks      :: Record Tasks
  , tree       :: FTree
  | CommonProps
  )

toHtmlFirstLevel :: R2.Component ToHtmlProps
toHtmlFirstLevel = R.createElement toHtmlFirstLevelCpt
  where
    -- TODO This shouldn't be here: make it a top-level function but be careful
    -- about cyclic defines
    -- https://discourse.purescript.org/t/strange-compiler-error-with-an-undefined-reference/2060/3
    toHtmlFirstLevelCpt :: R.Component ToHtmlProps
    toHtmlFirstLevelCpt = R.hooksComponentWithModule thisModule "toHtmlFirstLevel" cpt

    cpt p@{ appReload
          , asyncTasks
          , currentRoute
          , frontends
          , handed
          , openNodes
          , reload
          , reloadTree
          , session
          , tree: tree@(NTree (LNode { id
                                      , name
                                      , nodeType
                                      }
                              ) ary
                        )
          } _ = do
      setPopoverRef <- R.useRef Nothing

      let pAction a   = performAction a (RecordE.pick (Record.merge p { setPopoverRef }) :: Record PerformActionProps)

      let nodeId               = mkNodeId session id
      let folderIsOpen         = Set.member nodeId (fst openNodes)
      let setFn                = if folderIsOpen then Set.delete else Set.insert
      let toggleFolderIsOpen _ = (snd openNodes) (setFn nodeId)
      let folderOpen           = Tuple folderIsOpen toggleFolderIsOpen

      let withId (NTree (LNode {id: id'}) _) = id'

      pure $ H.li { className: if A.null ary then "no-children" else "with-children" } $
        [ nodeSpan { appReload
                   , asyncTasks
                   , currentRoute
                   , dispatch: pAction
                   , folderOpen
                   , frontends
                   , handed
                   , id
                   , isLeaf: A.null ary
                   , name
                   , nodeType
                   , session
                   , setPopoverRef
                   -- , tasks
                   }
          $ renderChildren folderOpen publicizedChildren
        ]
      where
        commonProps = RecordE.pick p :: Record CommonProps

        publicizedChildren = if isPublic nodeType
                              then map (\t -> map (\(LNode n@{ nodeType:nt } )
                                                  -> (LNode (n { nodeType = publicize nt }))
                                                  ) t) ary
                              else ary

        renderChildren (false /\ _) _ = []
        renderChildren folderOpen@(true /\ _) cs =
          (
            map (\t@(NTree (LNode {id: cId}) _) ->
                  childNodeFirstLevel ( Record.merge commonProps
                                        { asyncTasks
                                        , folderOpen
                                        , handed
                                        , id: cId
                                        , reloadTree
                                        }
                                      ) []
                ) $ sorted publicizedChildren
          )

    sorted :: Array FTree -> Array FTree
    sorted = A.sortWith (\(NTree (LNode {id}) _) -> id)


type ChildNodeFirstLevelProps = (
    asyncTasks   :: GAT.Reductor
  , folderOpen   :: R.State Boolean
  , id           :: ID
  , reloadTree   :: GUR.ReloadS
  | CommonProps
  )

childNodeFirstLevel :: R2.Component ChildNodeFirstLevelProps
childNodeFirstLevel = R.createElement childNodeFirstLevelCpt
  where
    -- TODO This shouldn't be here: make it a top-level function but be careful
    -- about cyclic defines
    -- https://discourse.purescript.org/t/strange-compiler-error-with-an-undefined-reference/2060/3
    childNodeFirstLevelCpt :: R.Component ChildNodeFirstLevelProps
    childNodeFirstLevelCpt = R.hooksComponentWithModule thisModule "childNodeFirstLevel" cpt

    cpt props@{ appReload
              , asyncTasks
              , currentRoute
              , folderOpen
              , id
              , frontends
              , handed
              , openNodes
              , reload
              , reloadTree
              , session } _ = do
      cptReload <- GUR.new

      let fetch _ = getNodeTreeFirstLevel session id
      let paint loaded = childNodeFirstLevelPaint { appReload
                                                  , asyncTasks
                                                  , currentRoute
                                                  , folderOpen
                                                  , frontends
                                                  , handed
                                                  , openNodes
                                                  , reload: cptReload
                                                  , reloadTree
                                                  , session
                                                  , tree: loaded } []

      useLoader { counter: GUR.value cptReload
                , root: id
                , treeCounter: GUR.value reloadTree } fetch paint


type ChildNodeFirstLevelPaintProps = (
    asyncTasks   :: GAT.Reductor
  , folderOpen   :: R.State Boolean
  , reloadTree   :: GUR.ReloadS
  , tree         :: FTree
  | CommonProps
  )

childNodeFirstLevelPaint :: R2.Component ChildNodeFirstLevelPaintProps
childNodeFirstLevelPaint = R.createElement childNodeFirstLevelPaintCpt
  where
    -- TODO This shouldn't be here: make it a top-level function but be careful
    -- about cyclic defines
    -- https://discourse.purescript.org/t/strange-compiler-error-with-an-undefined-reference/2060/3
    childNodeFirstLevelPaintCpt :: R.Component ChildNodeFirstLevelPaintProps
    childNodeFirstLevelPaintCpt = R.hooksComponentWithModule thisModule "childNodeFirstLevelPaint" cpt
    -- TODO folderOpen is unused

    cpt props@{ asyncTasks
              , handed
              , reload
              , reloadTree
              , tree: ctree@(NTree (LNode { id }) _) } _ = do
      pure $ H.ul {} [
        toHtmlFirstLevel (Record.merge commonProps { asyncTasks
                                                   , handed
                                                   , reloadTree
                                                   , tree: ctree }
                        ) []
        ]
      -- pure $ H.div { } [ H.text $ "[closed] Node id " <> show id ]
      where
        commonProps = RecordE.pick props :: Record CommonProps


type PerformActionProps = (
    appReload    :: GUR.ReloadS
  , asyncTasks   :: GAT.Reductor
  , openNodes    :: R.State OpenNodes
  , reload       :: GUR.ReloadS
  , reloadTree   :: GUR.ReloadS
  , session      :: Session
  , setPopoverRef :: R.Ref (Maybe (Boolean -> Effect Unit))
  -- , tasks     :: Record Tasks
  , tree         :: FTree
  )

-------
performAction :: Action
              -> Record PerformActionProps
              -> Aff Unit
performAction (DeleteNode nt) p@{ openNodes: (_ /\ setOpenNodes)
                                , session
                                , tree: (NTree (LNode {id, parent_id}) _)
                                } =
  do
    case nt of
         GT.NodePublic GT.FolderPublic -> void $ deleteNode session nt id
         GT.NodePublic _               -> void $ unpublishNode session parent_id id
         _                             -> void $ deleteNode session nt id

    liftEffect $ setOpenNodes (Set.delete (mkNodeId session id))
    performAction RefreshTree p

-------
performAction (DoSearch task) { asyncTasks: (_ /\ dispatch)
                              , session
                              , tree: (NTree (LNode {id}) _)
                              }  =
  do
    liftEffect $ dispatch $ GAT.Insert id task
    liftEffect $ log2 "[performAction] DoSearch task:" task

-------
performAction (UpdateNode params) { asyncTasks: (_ /\ dispatch)
                                  , session
                                  , tree: (NTree (LNode {id}) _)
                                  } =
  do
    task <- updateRequest params session id
    liftEffect $ dispatch $ GAT.Insert id task
    liftEffect $ log2 "[performAction] UpdateNode task:" task


-------
performAction (RenameNode name) p@{ session
                                  , tree: (NTree (LNode {id}) _)
                                  } =
  do
    void $ rename session id $ RenameValue {text:name}
    performAction RefreshTree p

-------
performAction (ShareTeam username) p@{ session
                                     , tree: (NTree (LNode {id}) _)
                                     } =
  do
    void $ Share.shareReq session id $ Share.ShareTeamParams {username}


performAction (SharePublic {params}) p@{ session
                                       , openNodes: (_ /\ setOpenNodes)
                                       } =
  case params of
    Nothing -> performAction NoAction p
    Just (SubTreeOut {in:inId,out}) -> do
      void $ Share.shareReq session inId $ Share.SharePublicParams {node_id:out}
      liftEffect $ setOpenNodes (Set.insert (mkNodeId session out))
      performAction RefreshTree p


performAction (AddContact params) p@{ session
                                     , tree: (NTree (LNode {id}) _)
                                     } =
    void $ Contact.contactReq session id params



-------
performAction (AddNode name nodeType) p@{ openNodes: (_ /\ setOpenNodes)
                                        , session
                                        , tree: (NTree (LNode {id}) _)
                                        } =
  do
    task <- addNode session id $ AddNodeValue {name, nodeType}
    liftEffect $ setOpenNodes (Set.insert (mkNodeId session id))
    performAction RefreshTree p

-------
performAction (UploadFile nodeType fileType mName blob) { asyncTasks: (_ /\ dispatch)
                                                        , session
                                                        , tree: (NTree (LNode {id}) _)
                                                        } =
  do
    task <- uploadFile session nodeType id fileType {mName, blob}
    liftEffect $ do
      dispatch $ GAT.Insert id task
      log2 "[performAction] UploadFile, uploaded, task:" task

performAction (UploadArbitraryFile mName blob) { asyncTasks: (_ /\ dispatch)
                                               , session
                                               , tree: (NTree (LNode {id}) _)
                                               } =
  do
    task <- uploadArbitraryFile session id { blob, mName }
    liftEffect $ do
      dispatch $ GAT.Insert id task
      log2 "[performAction] UploadArbitraryFile, uploaded, task:" task

-------
performAction DownloadNode _ = do
    liftEffect $ log "[performAction] DownloadNode"
-------
performAction (MoveNode {params}) p@{ openNodes: (_ /\ setOpenNodes)
                                    , session } =
  case params of
    Nothing -> performAction NoAction p
    Just (SubTreeOut {in:in',out}) -> do
      void $ moveNodeReq session in' out
      liftEffect $ setOpenNodes (Set.insert (mkNodeId session out))
      performAction RefreshTree p

performAction (MergeNode {params}) p@{session} =
  case params of
    Nothing -> performAction NoAction p
    Just (SubTreeOut {in:in',out}) -> do
      void $ mergeNodeReq session in' out
      performAction RefreshTree p

performAction (LinkNode {nodeType, params}) p@{session} = do
  case params of
    Nothing -> performAction NoAction p
    Just (SubTreeOut {in:in',out}) -> do
      void $ linkNodeReq session nodeType in' out
      performAction RefreshTree p

-------
performAction RefreshTree p@{ reloadTree
                            , setPopoverRef } = do
  liftEffect $ do
    GUR.bump reloadTree
  performAction ClosePopover p
-------
performAction NoAction _ = do
    liftEffect $ log "[performAction] NoAction"

performAction ClosePopover { setPopoverRef } = do
  liftEffect $ do
    case R.readRef setPopoverRef of
      Nothing -> pure unit
      Just setPopover -> setPopover false
