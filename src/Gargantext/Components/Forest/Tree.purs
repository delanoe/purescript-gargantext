module Gargantext.Components.Forest.Tree where

import Gargantext.Components.Forest.Tree.Node.Action
import Gargantext.Prelude

import DOM.Simple.Console (log2)
import Data.Array as A
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Gargantext.Components.Forest.Tree.Node.Action.Upload (uploadFile)
import Gargantext.Components.Forest.Tree.Node.Box (nodeMainSpan)
import Gargantext.Components.Loader (loader)
import Gargantext.Components.Login.Types (TreeId)
import Gargantext.Ends (Frontends)
import Gargantext.Routes (AppRoute)
import Gargantext.Sessions (Session)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H

------------------------------------------------------------------------
type Props = ( root          :: ID
             , mCurrentRoute :: Maybe AppRoute
             , session       :: Session
             , frontends     :: Frontends
             )

treeView :: Record Props -> R.Element
treeView props = R.createElement treeViewCpt props []

treeViewCpt :: R.Component Props
treeViewCpt = R.hooksComponent "G.C.Tree.treeView" cpt
  where
    cpt { root, mCurrentRoute, session, frontends } _children = do
      -- NOTE: this is a hack to reload the tree view on demand
      reload <- R.useState' (0 :: Reload)
      openNodes <- R2.useLocalStorageState R2.openNodesKey (Set.empty :: Set TreeId)
      pure $ treeLoadView reload
        { root, mCurrentRoute, session, frontends, openNodes }

type Props' = ( root          :: ID
              , mCurrentRoute :: Maybe AppRoute
              , session       :: Session
              , frontends     :: Frontends
              , openNodes     :: R.State (Set TreeId)
              )

treeLoadView :: R.State Reload -> Record Props' -> R.Element
treeLoadView reload p = R.createElement el p []
  where
    el = R.staticComponent "TreeLoadView" cpt
    cpt {root, mCurrentRoute, session, frontends, openNodes} _ = do
      loader root (loadNode session) $ \loaded ->
        loadedTreeView reload {tree: loaded, mCurrentRoute, session, frontends, openNodes}

type TreeViewProps = ( tree          :: FTree
                     , mCurrentRoute :: Maybe AppRoute
                     , frontends     :: Frontends
                     , session       :: Session 
                     , openNodes     :: R.State (Set TreeId)
                     )

loadedTreeView :: R.State Reload -> Record TreeViewProps -> R.Element
loadedTreeView reload p = R.createElement el p []
  where
    el = R.hooksComponent "LoadedTreeView" cpt
    cpt {tree, mCurrentRoute, session, frontends, openNodes} _ = do
      treeState <- R.useState' {tree, asyncTasks: []}

      pure $ H.div {className: "tree"}
        [ toHtml reload treeState session frontends mCurrentRoute openNodes ]

------------------------------------------------------------------------
toHtml :: R.State Reload
       -> R.State Tree
       -> Session
       -> Frontends
       -> Maybe AppRoute
       -> R.State (Set TreeId)
       -> R.Element
toHtml reload treeState@(ts@{tree: (NTree (LNode {id, name, nodeType}) ary), asyncTasks} /\ setTreeState) session frontends mCurrentRoute openNodes = R.createElement el {} []
  where
    el = R.hooksComponent "NodeView" cpt
    pAction = performAction session reload treeState

    cpt props _ = do
      let folderIsOpen = Set.member id (fst openNodes)
      let setFn = if folderIsOpen then Set.delete else Set.insert
      let toggleFolderIsOpen _ = (snd openNodes) (setFn id)
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

    onAsyncTaskFinish (GT.AsyncTaskWithType {task: GT.AsyncTask {id}}) = setTreeState $ const $ ts { asyncTasks = newAsyncTasks }
      where
        newAsyncTasks = A.filter (\(GT.AsyncTaskWithType {task: GT.AsyncTask {id: id'}}) -> id /= id') asyncTasks


childNodes :: Session
           -> Frontends
           -> R.State Reload
           -> R.State Boolean
           -> Maybe AppRoute
           -> R.State (Set TreeId)
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
        treeState <- R.useState' {tree, asyncTasks}
        pure $ toHtml reload treeState session frontends mCurrentRoute openNodes

performAction :: Session
              -> R.State Int
              -> R.State Tree
              -> Action
              -> Aff Unit
performAction session (_ /\ setReload) (s@{tree: NTree (LNode {id}) _} /\ setTree) (CreateSubmit name nodeType) = do
  void $ createNode session id $ CreateValue {name, nodeType}
  liftEffect $ setReload (_ + 1)

performAction session (_ /\ setReload) (s@{tree: NTree (LNode {id}) _} /\ setTree) DeleteNode = do
  void $ deleteNode session id
  liftEffect $ setReload (_ + 1)

performAction session _ ({tree: NTree (LNode {id}) _} /\ setTree) (SearchQuery task) = do
  liftEffect $ setTree $ \t@{asyncTasks} -> t { asyncTasks = A.cons task asyncTasks }
  liftEffect $ log2 "search query, task:" task

performAction session _ ({tree: NTree (LNode {id}) _} /\ setTree) (Submit name)  = do
  void $ renameNode session id $ RenameValue {name}
  liftEffect $ setTree $ \s@{tree: NTree (LNode node) arr} -> s {tree = NTree (LNode node {name = name}) arr}

performAction session _ ({tree: NTree (LNode {id}) _} /\ setTree) (UploadFile fileType contents) = do
  task <- uploadFile session id fileType contents
  liftEffect $ setTree $ \t@{asyncTasks} -> t { asyncTasks = A.cons task asyncTasks }
  liftEffect $ log2 "uploaded, task:" task
