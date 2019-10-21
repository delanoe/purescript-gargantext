module Gargantext.Components.Forest.Tree where

import DOM.Simple.Console (log2)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Array (filter, null)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, launchAff, runAff)
import Effect.Class (liftEffect)
import Effect.Uncurried (mkEffectFn1)
import FFI.Simple ((..))
import Gargantext.Components.Forest.Tree.Node.Action
import Gargantext.Components.Forest.Tree.Node.Action.Add
import Gargantext.Components.Forest.Tree.Node.Action.Rename
import Gargantext.Components.Forest.Tree.Node.Action.Upload
import Gargantext.Components.Forest.Tree.Node
import Gargantext.Components.Forest.Tree.Node.Box
import Gargantext.Ends (Frontends)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (AppRoute)
import Gargantext.Sessions (Session)
import Prelude (Unit, bind, discard, map, pure, void, ($), (+), (<>))
import Reactix as R
import Reactix.DOM.HTML as H

------------------------------------------------------------------------
type Props = ( root          :: ID
             , mCurrentRoute :: Maybe AppRoute
             , session       :: Session
             , frontends     :: Frontends
             )
-----------------
treeView :: Record Props -> R.Element
treeView props = R.createElement treeViewCpt props []

treeViewCpt :: R.Component Props
treeViewCpt = R.hooksComponent "G.C.Tree.treeView" cpt
  where
    cpt props _children = do
      -- NOTE: this is a hack to reload the tree view on demand
      reload <- R.useState' (0 :: Reload)
      pure $ treeLoadView reload props

treeLoadView :: R.State Reload -> Record Props -> R.Element
treeLoadView reload p = R.createElement el p []
  where
    el = R.hooksComponent "TreeLoadView" cpt
    cpt {root, mCurrentRoute, session, frontends} _ = do
      useLoader root (loadNode session) $ \loaded ->
        loadedTreeView reload {tree: loaded, mCurrentRoute, session, frontends}

type TreeViewProps = ( tree          :: FTree
                     , mCurrentRoute :: Maybe AppRoute
                     , frontends     :: Frontends
                     , session       :: Session 
                     )

loadedTreeView :: R.State Reload -> Record TreeViewProps -> R.Element
loadedTreeView reload p = R.createElement el p []
  where
    el = R.hooksComponent "LoadedTreeView" cpt
    cpt {tree, mCurrentRoute, session, frontends} _ = do
      treeState <- R.useState' {tree}

      pure $ H.div {className: "tree"}
        [ toHtml reload treeState session frontends mCurrentRoute ]

------------------------------------------------------------------------
toHtml :: R.State Reload
       -> R.State Tree
       -> Session
       -> Frontends
       -> Maybe AppRoute
       -> R.Element
toHtml reload treeState@({tree: (NTree (LNode {id, name, nodeType}) ary)} /\ _) session frontends mCurrentRoute = R.createElement el {} []
  where
    el = R.hooksComponent "NodeView" cpt
    pAction = performAction session reload treeState

    cpt props _ = do
      folderOpen <- R.useState' true

      let withId (NTree (LNode {id: id'}) _) = id'

      pure $ H.ul {}
        [ H.li {}
          ( [ nodeMainSpan pAction {id, name, nodeType, mCurrentRoute} folderOpen session frontends ]
            <> childNodes session frontends reload folderOpen mCurrentRoute ary
          )
        ]


childNodes :: Session
           -> Frontends
           -> R.State Reload
           -> R.State Boolean
           -> Maybe AppRoute
           -> Array FTree
           -> Array R.Element
childNodes _ _ _ _ _ [] = []
childNodes _ _ _ (false /\ _) _ _ = []
childNodes session frontends reload (true /\ _) mCurrentRoute ary =
  map (\ctree -> childNode {tree: ctree}) ary
    where
      childNode :: Tree -> R.Element
      childNode props = R.createElement el props []
      el = R.hooksComponent "ChildNodeView" cpt
      cpt {tree} _ = do
        treeState <- R.useState' {tree}
        pure $ toHtml reload treeState session frontends mCurrentRoute


performAction :: Session
              -> R.State Int
              -> R.State Tree
              -> Action
              -> Aff Unit
performAction session (_ /\ setReload) (s@{tree: NTree (LNode {id}) _} /\ setTree) DeleteNode = do
  void $ deleteNode session id
  liftEffect $ setReload (_ + 1)

performAction session _ ({tree: NTree (LNode {id}) _} /\ setTree) (Submit name)  = do
  void $ renameNode session id $ RenameValue {name}
  liftEffect $ setTree $ \s@{tree: NTree (LNode node) arr} -> s {tree = NTree (LNode node {name = name}) arr}

performAction session (_ /\ setReload) (s@{tree: NTree (LNode {id}) _} /\ setTree) (CreateSubmit name nodeType) = do
  void $ createNode session id $ CreateValue {name, nodeType}
  liftEffect $ setReload (_ + 1)

performAction session _ ({tree: NTree (LNode {id}) _} /\ _) (UploadFile fileType contents) = do
  hashes <- uploadFile session id fileType contents
  liftEffect $ log2 "uploaded:" hashes

