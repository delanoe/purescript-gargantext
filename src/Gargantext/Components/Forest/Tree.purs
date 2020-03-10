module Gargantext.Components.Forest.Tree where

import DOM.Simple.Console (log2)
import Data.Array as A
import Data.Maybe (Maybe)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Gargantext.Components.Forest.Tree.Node.Action (Action(..), CreateValue(..), FTree, ID, LNode(..), NTree(..), Reload, RenameValue(..), Tree, createNode, deleteNode, loadNode, renameNode)
import Gargantext.Components.Forest.Tree.Node.Action.Upload (uploadFile)
import Gargantext.Components.Forest.Tree.Node.Box (nodeMainSpan)
import Gargantext.Ends (Frontends)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Prelude (Unit, bind, const, discard, map, pure, void, ($), (+), (/=), (<>))
import Gargantext.Routes (AppRoute)
import Gargantext.Sessions (OpenNodes, Session, mkNodeId)
import Gargantext.Types as GT
import Reactix as R
import Reactix.DOM.HTML as H

type CommonProps =
  (
      frontends :: Frontends
    , mCurrentRoute :: Maybe AppRoute
    , openNodes :: R.State OpenNodes
    , reload :: R.State Reload
    , session :: Session
  )

------------------------------------------------------------------------
type Props = ( root          :: ID
             | CommonProps
             )

treeView :: Record Props -> R.Element
treeView props = R.createElement treeViewCpt props []

treeViewCpt :: R.Component Props
treeViewCpt = R.hooksComponent "G.C.Tree.treeView" cpt
  where
    cpt { root, mCurrentRoute, session, frontends, openNodes, reload } _children = do
      pure $ treeLoadView
        { root, mCurrentRoute, session, frontends, openNodes, reload }

treeLoadView :: Record Props -> R.Element
treeLoadView p = R.createElement treeLoadView' p []

treeLoadView' :: R.Component Props
treeLoadView' = R.hooksComponent "TreeLoadView" cpt
  where
    cpt {root, mCurrentRoute, session, frontends, openNodes, reload} _ = do
      let fetch _ = loadNode session root
      let paint loaded = loadedTreeView {tree: loaded, mCurrentRoute, session, frontends, openNodes, reload}
      useLoader {root, counter: fst reload} fetch paint

type TreeViewProps = ( tree          :: FTree
                     | CommonProps
                     )


loadedTreeView :: Record TreeViewProps -> R.Element
loadedTreeView p = R.createElement loadedTreeView' p []

loadedTreeView' :: R.Component TreeViewProps
loadedTreeView' = R.hooksComponent "LoadedTreeView" cpt
  where
    cpt {tree, mCurrentRoute, session, frontends, openNodes, reload} _ = do
      tasks <- R.useState' []

      pure $ H.div {className: "tree"}
        [ toHtml { frontends, mCurrentRoute, openNodes, reload, session, tasks, tree } ]

------------------------------------------------------------------------

type ToHtmlProps =
  (
    tasks :: R.State (Array GT.AsyncTaskWithType)
  , tree :: FTree
  | CommonProps
  )

toHtml :: Record ToHtmlProps -> R.Element
toHtml { frontends
       , mCurrentRoute
       , openNodes
       , reload: reload@(_ /\ setReload)
       , session
       , tasks: tasks@(asyncTasks /\ setAsyncTasks)
       , tree: tree@(NTree (LNode {id, name, nodeType}) ary) } = R.createElement el {} []
  where
    el = R.hooksComponent "NodeView" cpt
    pAction = performAction {openNodes, reload, session, tasks, tree}

    cpt props _ = do
      let nodeId = mkNodeId session id
      let folderIsOpen = Set.member nodeId (fst openNodes)
      let setFn = if folderIsOpen then Set.delete else Set.insert
      let toggleFolderIsOpen _ = (snd openNodes) (setFn nodeId)
      let folderOpen = Tuple folderIsOpen toggleFolderIsOpen

      let withId (NTree (LNode {id: id'}) _) = id'

      pure $ H.ul {}
        [ H.li {}
          ( [ nodeMainSpan pAction { id
                                   , asyncTasks
                                   , mCurrentRoute
                                   , name
                                   , nodeType
                                   , onAsyncTaskFinish
                                   } folderOpen session frontends ]
            <> childNodes {children: ary, folderOpen, frontends, mCurrentRoute, openNodes, reload, session }
          )
        ]

    onAsyncTaskFinish (GT.AsyncTaskWithType {task: GT.AsyncTask {id: id'}}) = do
      setAsyncTasks $ const newAsyncTasks
      setReload (_ + 1)
      where
        newAsyncTasks = A.filter (\(GT.AsyncTaskWithType {task: GT.AsyncTask {id: id''}}) -> id' /= id'') asyncTasks


type ChildNodesProps =
  (
      children :: Array FTree
    , folderOpen :: R.State Boolean
    | CommonProps
  )


childNodes :: Record ChildNodesProps -> Array R.Element
childNodes { children: [] } = []
childNodes { folderOpen: (false /\ _) } = []
childNodes { children, folderOpen: (true /\ _), frontends, mCurrentRoute, openNodes, reload, session } =
  map (\ctree -> childNode {tree: ctree, asyncTasks: []}) $ sorted children
    where
      sorted :: Array FTree -> Array FTree
      sorted = A.sortWith (\(NTree (LNode {id}) _) -> id)
      childNode :: Tree -> R.Element
      childNode props = R.createElement el props []
      el = R.hooksComponent "ChildNodeView" cpt
      cpt {tree, asyncTasks} _ = do
        tasks <- R.useState' asyncTasks
        pure $ toHtml { frontends, mCurrentRoute, openNodes, reload, session, tasks, tree }

performAction :: { openNodes :: R.State OpenNodes
                 , reload :: R.State Reload
                 , session :: Session
                 , tasks :: R.State (Array GT.AsyncTaskWithType)
                 , tree :: FTree }
              -> Action
              -> Aff Unit
performAction { openNodes: (_ /\ setOpenNodes)
              , reload: (_ /\ setReload)
              , session
              , tree: (NTree (LNode {id}) _) } DeleteNode = do
  void $ deleteNode session id
  liftEffect do
    setOpenNodes (Set.delete (mkNodeId session id))
    setReload (_ + 1)

performAction { reload: (_ /\ setReload)
              , session
              , tasks: (_ /\ setAsyncTasks)
              , tree: (NTree (LNode {id}) _) } (SearchQuery task) = do
  liftEffect $ setAsyncTasks $ A.cons task
  liftEffect $ log2 "[performAction] SearchQuery task:" task
  liftEffect $ setReload (_ + 1)

performAction { reload: (_ /\ setReload)
              , session
              , tree: (NTree (LNode {id}) _) } (Submit name)  = do
  void $ renameNode session id $ RenameValue {name}
  liftEffect do
    setReload (_ + 1)

performAction { openNodes: (_ /\ setOpenNodes)
              , reload: (_ /\ setReload)
              , session
              , tree: (NTree (LNode {id}) _) } (CreateSubmit name nodeType) = do
  void $ createNode session id $ CreateValue {name, nodeType}
  liftEffect do
    setOpenNodes (Set.insert (mkNodeId session id))
    setReload (_ + 1)

performAction { session
              , tasks: (_ /\ setAsyncTasks)
              , tree: (NTree (LNode {id}) _) } (UploadFile nodeType fileType mName contents) = do
  task <- uploadFile session nodeType id fileType {mName, contents}
  liftEffect $ setAsyncTasks $ A.cons task
  liftEffect $ log2 "uploaded, task:" task
