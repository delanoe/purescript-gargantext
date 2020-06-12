module Gargantext.Components.Forest.Tree where

import DOM.Simple.Console (log2)
import Data.Array as A
import Data.Maybe (Maybe)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Gargantext.AsyncTasks as GAT
import Gargantext.Components.Forest.Tree.Node.Action (Action(..))
import Gargantext.Components.Forest.Tree.Node.Action.Add (AddNodeValue(..), addNode)
import Gargantext.Components.Forest.Tree.Node.Action.CopyFrom (loadNode)
import Gargantext.Components.Forest.Tree.Node.Action.Delete (deleteNode)
import Gargantext.Components.Forest.Tree.Node.Action.Rename (RenameValue(..), rename)
import Gargantext.Components.Forest.Tree.Node.Action.Share (ShareValue(..), share)
import Gargantext.Components.Forest.Tree.Node.Action.Upload (uploadFile)
import Gargantext.Components.Forest.Tree.Node.Box (nodeMainSpan, Tasks, tasksStruct)
import Gargantext.Components.Forest.Tree.Node.FTree (FTree, LNode(..), NTree(..))
import Gargantext.Ends (Frontends)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Prelude (Unit, bind, discard, map, pure, void, ($), (+), (<>))
import Gargantext.Routes (AppRoute)
import Gargantext.Sessions (OpenNodes, Session, mkNodeId)
import Gargantext.Types (ID, Reload)
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Record.Extra as RecordE

------------------------------------------------------------------------
type CommonProps =
  ( frontends     :: Frontends
  , mCurrentRoute :: Maybe AppRoute
  , openNodes     :: R.State OpenNodes
  , reload        :: R.State Reload
  , session       :: Session
  )

------------------------------------------------------------------------
type Props = ( root          :: ID
             , asyncTasks    :: R.State GAT.Storage
             | CommonProps
             )

treeView :: Record Props -> R.Element
treeView props = R.createElement treeViewCpt props []
  where
    treeViewCpt :: R.Component Props
    treeViewCpt = R.hooksComponent "G.C.Tree.treeView" cpt
      where
        cpt { root, mCurrentRoute, session, frontends, openNodes, reload, asyncTasks} _children = do
          pure $ treeLoadView
            { root, mCurrentRoute, session, frontends, openNodes, reload, asyncTasks}

treeLoadView :: Record Props -> R.Element
treeLoadView p = R.createElement treeLoadViewCpt p []
  where
    treeLoadViewCpt :: R.Component Props
    treeLoadViewCpt = R.hooksComponent "TreeLoadView" cpt
      where
        cpt { root, asyncTasks, mCurrentRoute, session, frontends, openNodes, reload } _children = do
          let fetch _ = loadNode session root
          let paint loaded = loadedTreeView { asyncTasks
                                            , frontends
                                            , mCurrentRoute
                                            , openNodes
                                            , reload
                                            , session
                                            , tasks: tasksStruct root asyncTasks reload
                                            , tree: loaded
                                            }
          useLoader { root, counter: fst reload } fetch paint

type TreeViewProps = ( asyncTasks    :: R.State GAT.Storage
                     , tree          :: FTree
                     , tasks         :: Record Tasks
                     | CommonProps
                     )

loadedTreeView :: Record TreeViewProps -> R.Element
loadedTreeView p = R.createElement loadedTreeViewCpt p []
  where
    loadedTreeViewCpt :: R.Component TreeViewProps
    loadedTreeViewCpt = R.hooksComponent "LoadedTreeView" cpt
      where
        cpt { asyncTasks, frontends, mCurrentRoute, openNodes, reload, tasks, tree, session } _ = do
          pure $ H.div {className: "tree"}
            [ toHtml { asyncTasks, frontends, mCurrentRoute, openNodes, reload, session, tasks, tree } ]

------------------------------------------------------------------------
type ToHtmlProps =
  (
    asyncTasks :: R.State GAT.Storage
  , tasks :: Record Tasks
  , tree :: FTree
  | CommonProps
  )

toHtml :: Record ToHtmlProps -> R.Element
toHtml p@{ asyncTasks
         , frontends
         , mCurrentRoute
         , openNodes
         , reload: reload@(_ /\ setReload)
         , session
         , tasks: tasks@{ onTaskAdd, onTaskFinish, tasks: tasks' }
         , tree: tree@(NTree (LNode {id, name, nodeType}) ary) } = R.createElement el {} []
  where
    el          = R.hooksComponent "NodeView" cpt
    commonProps = RecordE.pick p :: Record CommonProps
    pAction a   = performAction a (RecordE.pick p :: Record PerformActionProps)

    cpt _ _ = do
      let nodeId               = mkNodeId session id
      let folderIsOpen         = Set.member nodeId (fst openNodes)
      let setFn                = if folderIsOpen then Set.delete else Set.insert
      let toggleFolderIsOpen _ = (snd openNodes) (setFn nodeId)
      let folderOpen           = Tuple folderIsOpen toggleFolderIsOpen

      let withId (NTree (LNode {id: id'}) _) = id'

      pure $ H.ul {}
        [ H.li {}
          ( [ nodeMainSpan { id
                           , dispatch: pAction
                           , folderOpen
                           , frontends
                           , mCurrentRoute
                           , name
                           , nodeType
                           , session
                           , tasks
                           } ]
            <> childNodes (Record.merge commonProps
                                        { asyncTasks
                                        , children: ary
                                        , folderOpen })
          )
        ]


type ChildNodesProps =
  ( asyncTasks :: R.State GAT.Storage
  , children :: Array FTree
  , folderOpen :: R.State Boolean
  | CommonProps
  )

childNodes :: Record ChildNodesProps -> Array R.Element
childNodes { children: [] } = []
childNodes { folderOpen: (false /\ _) } = []
childNodes props@{ asyncTasks, children, reload } =
  map (\ctree@(NTree (LNode {id}) _) ->
        toHtml (Record.merge commonProps {
                     asyncTasks
                   , tasks: tasksStruct id asyncTasks reload
                   , tree: ctree
                   })) $ sorted children
  where
    commonProps = RecordE.pick props :: Record CommonProps
    sorted :: Array FTree -> Array FTree
    sorted = A.sortWith (\(NTree (LNode {id}) _) -> id)

type PerformActionProps =
  ( openNodes :: R.State OpenNodes
  , reload    :: R.State Reload
  , session   :: Session
  , tasks     :: Record Tasks
  , tree      :: FTree
  )

performAction :: Action
              -> Record PerformActionProps
              -> Aff Unit
performAction DeleteNode p@{ openNodes: (_ /\ setOpenNodes)
                           , reload: (_ /\ setReload)
                           , session
                           , tree: (NTree (LNode {id}) _)
                           } =
  do
    void $ deleteNode session id
    liftEffect do
      setOpenNodes (Set.delete (mkNodeId session id))
    performAction RefreshTree p

performAction (DoSearch task) { reload: (_ /\ setReload)
                              , session
                              , tasks: { onTaskAdd }
                              , tree: (NTree (LNode {id}) _)
                              }  =
  do
    liftEffect $ onTaskAdd task
    liftEffect $ log2 "[performAction] DoSearch task:" task

performAction (UpdateNode task) { reload: (_ /\ setReload)
                                , session
                                , tasks: {onTaskAdd}
                                , tree: (NTree (LNode {id}) _)
                                } =
  do
    liftEffect $ onTaskAdd task
    liftEffect $ log2 "[performAction] UpdateNode task:" task

performAction (RenameNode name) p@{ reload: (_ /\ setReload)
                                , session
                                , tree: (NTree (LNode {id}) _)
                                } =
  do
    void $ rename session id $ RenameValue {text:name}
    performAction RefreshTree p

performAction (ShareNode username) p@{ reload: (_ /\ setReload)
                                     , session
                                     , tree: (NTree (LNode {id}) _)
                                     } =
  do
    void $ share session id $ ShareValue {text:username}

performAction (AddNode name nodeType) p@{ openNodes: (_ /\ setOpenNodes)
                                        , reload:    (_ /\ setReload)
                                        , session
                                        , tree: (NTree (LNode {id}) _)
                                        } =
  do
    task <- addNode session id $ AddNodeValue {name, nodeType}
    liftEffect do
      setOpenNodes (Set.insert (mkNodeId session id))
    performAction RefreshTree p

performAction (UploadFile nodeType fileType mName contents) { session
                                                            , tasks: { onTaskAdd }
                                                            , tree: (NTree (LNode {id}) _)
                                                            } =
  do
    task <- uploadFile session nodeType id fileType {mName, contents}
    liftEffect $ onTaskAdd task
    liftEffect $ log2 "uploaded, task:" task

performAction RefreshTree { reload: (_ /\ setReload) } = do
  liftEffect $ setReload (_ + 1)
