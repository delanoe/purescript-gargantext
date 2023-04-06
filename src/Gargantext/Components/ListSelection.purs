module Gargantext.Components.ListSelection where

import Gargantext.Prelude

import Data.Array as A
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Forest.Tree.Node.Tools (formChoiceSafe)
import Gargantext.Components.ListSelection.Types (NodeSimple(..), Selection(..), selectedListIds)
import Gargantext.Config.REST (RESTError(..), AffRESTError)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session(..), get)
import Gargantext.Types (ID, NodeType(..), fldr)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.ListSelection"

type Props =
  ( selection :: T.Box Selection
  , session   :: Session
  )

selection :: R2.Component Props
selection = R.createElement selectionCpt
selectionCpt :: R.Component Props
selectionCpt = here.component "selection" cpt where
  cpt { selection, session } _ = do
    selection' <- R2.useLive' selection
    pure $ H.div { className: "list-selection" }
      [
        B.formSelect'
        { callback: flip T.write_ selection
        , value: selection'
        , list: [ MyListsFirst
                , OtherListsFirst
                , SelectedLists []
                , NoList
                ]
        }
        []
      , selectedIds { selection, session } []
      ]

selectedIds :: R2.Component Props
selectedIds = R.createElement selectedIdsCpt
selectedIdsCpt :: R.Component Props
selectedIdsCpt = here.component "selectedIds" cpt where
  cpt { selection, session } _ = do
    selection' <- T.useLive T.unequal selection

    pure $ case selection' of
      SelectedLists ids -> H.div {} [ idsSelector { selection, session } [] ]
      _ -> H.div {} []

type IdsSelectorProps =
  ( selection :: T.Box Selection
  , session   :: Session )

idsSelector :: R2.Component IdsSelectorProps
idsSelector = R.createElement idsSelectorCpt
idsSelectorCpt :: R.Component IdsSelectorProps
idsSelectorCpt = here.component "idsSelector" cpt where
  cpt { selection, session } _ = do
    pure $ H.div { className: "ids-selector" }
      [ listTree { name: "", nodeType: NodeUser, root, selection, session } ] -- $ map checkbox [1, 2, 3, 4]
    where
      Session { treeId: root } = session

listIdsRoute :: ID -> SessionRoute
listIdsRoute = Children NodeList 0 1 Nothing <<< Just

treeFirstLevelRoute :: ID -> SessionRoute
treeFirstLevelRoute id = TreeFirstLevel (Just id) ""

loadTreeChildren :: { root :: ID, session :: Session } -> AffRESTError (Array NodeSimple)
loadTreeChildren { root, session } = do
  eResult :: (Either RESTError { children :: Array NodeSimple }) <- get session $ treeFirstLevelRoute root
  pure $ (\{ children } -> children) <$> eResult

type ListTreeProps =
  ( name      :: String
  , nodeType  :: NodeType
  , root      :: ID
  , selection :: T.Box Selection
  , session   :: Session
  )

listTree :: R2.Leaf ListTreeProps
listTree props = R.createElement listTreeCpt props []
listTreeCpt :: R.Component ListTreeProps
listTreeCpt = here.component "listTree" cpt where
  cpt { name, nodeType, root, selection, session } _ = do
    pure $ H.div { className: "tree" }
      [ H.div { className: "root" }
        [ H.i { className: fldr nodeType true } []
        , H.text $ "[" <> show root <> "] " <> name ]
      , listTreeChildren { render: listTree
                         , root
                         , selection
                         , session } []
      ]

type Render = Record ListTreeProps -> R.Element
type ListTreeChildrenProps =
  ( render    :: Render
  , root      :: ID
  , selection :: T.Box Selection
  , session   :: Session
  )

listTreeChildren :: R2.Component ListTreeChildrenProps
listTreeChildren = R.createElement listTreeChildrenCpt
listTreeChildrenCpt :: R.Component ListTreeChildrenProps
listTreeChildrenCpt = here.component "listTreeChildren" cpt where
  cpt { render, root, selection, session } _ = do
    useLoader { errorHandler
              , loader: loadTreeChildren
              , path: { root, session }
              , render: \loaded ->
                  listTreeChildrenLoaded { loaded
                                         , render
                                         , root
                                         , selection
                                         , session } [] }
    where
      errorHandler err = case err of
        ReadJSONError err' -> here.warn2 "[listTreeChildren] ReadJSONError" $ show err'
        _                  -> here.warn2 "[listTreeChildren] RESTError" err

type ListTreeChildrenLoadedProps =
  ( loaded    :: Array NodeSimple
  , render    :: Render
  , root      :: ID
  , selection :: T.Box Selection
  , session   :: Session )

listTreeChildrenLoaded :: R2.Component ListTreeChildrenLoadedProps
listTreeChildrenLoaded = R.createElement listTreeChildrenLoadedCpt
listTreeChildrenLoadedCpt :: R.Component ListTreeChildrenLoadedProps
listTreeChildrenLoadedCpt = here.component "listTreeChildrenLoaded" cpt where
  cpt { loaded, render, root, selection, session } _  = do
    pure $ H.div { className: "children" } (element <$> loaded)
    where
      element (NodeSimple { id, name, nodeType: nodeType@Corpus }) =
        render { root: id, name, nodeType, selection, session }
      element (NodeSimple { id, name, nodeType: nodeType@Folder }) =
        render { root: id, name, nodeType, selection, session }
      element (NodeSimple { id, name, nodeType: nodeType@FolderPrivate }) =
        render { root: id, name, nodeType, selection, session }
      element (NodeSimple { id, name, nodeType: nodeType@FolderPublic }) =
        render { root: id, name, nodeType, selection, session }
      element (NodeSimple { id, name, nodeType: nodeType@FolderShared }) =
        render { root: id, name, nodeType, selection, session }
      element (NodeSimple { id, name, nodeType: NodeList}) =
        renderListElement { id, name, selection }
      element (NodeSimple { id, name, nodeType: nodeType@Team }) =
        render { root: id, name, nodeType, selection, session }
      element _ = H.div {} []

type RenderListElementProps =
  ( id        :: ID
  , name      :: String
  , selection :: T.Box Selection )

renderListElement :: R2.Leaf RenderListElementProps
renderListElement = R2.leaf renderListElementCpt
renderListElementCpt :: R.Component RenderListElementProps
renderListElementCpt = here.component "renderListElement" cpt where
  cpt { id, name, selection } _ = do
    selection' <- T.useLive T.unequal selection

    let ids = selectedListIds selection'

    pure $ H.div { className: "leaf" }
      [ H.input { defaultChecked: A.elem id ids
                , on: { click: click ids }
                , type: "checkbox" }
      , H.i { className: fldr NodeList true } []
      , H.text $ "[" <> show id <> "] " <> name
      ]
      where
        click ids _ = do
          let f (SelectedLists lst) =
                if A.elem id ids
                then SelectedLists (A.delete id lst)
                else SelectedLists (A.cons id lst)
              f x = x
          T.modify_ f selection
