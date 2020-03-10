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

------------------------------------------------------------------------
type Props = ( root          :: ID
             , mCurrentRoute :: Maybe AppRoute
             , session       :: Session
             , frontends     :: Frontends
             , openNodes     :: R.State OpenNodes
             , reload        :: R.State Reload
             )

treeView :: Record Props -> R.Element
treeView props = R.createElement treeViewCpt props []

treeViewCpt :: R.Component Props
treeViewCpt = R.hooksComponent "G.C.Tree.treeView" cpt
  where
    cpt { root, mCurrentRoute, session, frontends, openNodes, reload } _children = do
      pure $ treeLoadView
        { root, mCurrentRoute, session, frontends, openNodes, reload }

type Props' = ( root          :: ID
              , mCurrentRoute :: Maybe AppRoute
              , session       :: Session
              , frontends     :: Frontends
              , openNodes     :: R.State OpenNodes
              , reload        :: R.State Reload
              )

treeLoadView :: Record Props' -> R.Element
treeLoadView p = R.createElement treeLoadView' p []

treeLoadView' :: R.Component Props'
treeLoadView' = R.hooksComponent "TreeLoadView" cpt
  where
    cpt {root, mCurrentRoute, session, frontends, openNodes, reload} _ = do
      let fetch _ = loadNode session root
      let paint loaded = loadedTreeView {tree: loaded, mCurrentRoute, session, frontends, openNodes, reload}
      useLoader {root, counter: fst reload} fetch paint

type TreeViewProps = ( tree          :: FTree
                     , mCurrentRoute :: Maybe AppRoute
                     , frontends     :: Frontends
                     , session       :: Session
                     , openNodes     :: R.State OpenNodes
                     , reload :: R.State Reload
                     )


loadedTreeView :: Record TreeViewProps -> R.Element
loadedTreeView p = R.createElement loadedTreeView' p []

loadedTreeView' :: R.Component TreeViewProps
loadedTreeView' = R.hooksComponent "LoadedTreeView" cpt
  where
    cpt {tree, mCurrentRoute, session, frontends, openNodes, reload} _ = do
      tasks <- R.useState' []

      pure $ H.div {className: "tree"}
        [ toHtml reload tree tasks session frontends mCurrentRoute openNodes ]

------------------------------------------------------------------------
toHtml :: R.State Reload
       -> FTree
       -> R.State (Array GT.AsyncTaskWithType)
       -> Session
       -> Frontends
       -> Maybe AppRoute
       -> R.State OpenNodes
       -> R.Element
toHtml reload@(_ /\ setReload) tree@(NTree (LNode {id, name, nodeType}) ary) tasks@(asyncTasks /\ setAsyncTasks) session frontends mCurrentRoute openNodes = R.createElement el {} []
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
            <> childNodes session frontends reload folderOpen mCurrentRoute openNodes ary
          )
        ]

    onAsyncTaskFinish (GT.AsyncTaskWithType {task: GT.AsyncTask {id: id'}}) = do
      setAsyncTasks $ const newAsyncTasks
      setReload (_ + 1)
      where
        newAsyncTasks = A.filter (\(GT.AsyncTaskWithType {task: GT.AsyncTask {id: id''}}) -> id' /= id'') asyncTasks


childNodes :: Session
           -> Frontends
           -> R.State Reload
           -> R.State Boolean
           -> Maybe AppRoute
           -> R.State OpenNodes
           -> Array FTree
           -> Array R.Element
childNodes _ _ _ _ _ _ [] = []
childNodes _ _ _ (false /\ _) _ _ _ = []
childNodes session frontends reload (true /\ _) mCurrentRoute openNodes ary =
  map (\ctree -> childNode {tree: ctree, asyncTasks: []}) $ sorted ary
    where
      sorted :: Array FTree -> Array FTree
      sorted = A.sortWith (\(NTree (LNode {id}) _) -> id)
      childNode :: Tree -> R.Element
      childNode props = R.createElement el props []
      el = R.hooksComponent "ChildNodeView" cpt
      cpt {tree, asyncTasks} _ = do
        tasks <- R.useState' asyncTasks
        pure $ toHtml reload tree tasks session frontends mCurrentRoute openNodes

performAction :: { openNodes :: R.State OpenNodes
                 , reload :: R.State Int
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
