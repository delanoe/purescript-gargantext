module Gargantext.Components.Forest.Tree.Node.Tools.SubTree where

import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import React.SyntheticEvent     as E
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Prelude

import Gargantext.Components.Forest.Tree.Node.Action (Props, Action, subTreeOut, setTreeOut)
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree.Types (SubTreeParams(..), SubTreeOut(..))
import Gargantext.Components.Forest.Tree.Node.Tools.FTree (FTree, LNode(..), NTree(..))
import Gargantext.Components.Forest.Tree.Node.Tools (nodeText)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session(..), get)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Tools.SubTree"

type SubTreeParamsIn =
  ( subTreeParams :: SubTreeParams
  , handed    :: GT.Handed
  | Props
  )

------------------------------------------------------------------------
type SubTreeParamsProps =
  ( action    :: R.State Action
  | SubTreeParamsIn
  )

subTreeView :: Record SubTreeParamsProps -> R.Element
subTreeView props = R.createElement subTreeViewCpt props []

subTreeViewCpt :: R.Component SubTreeParamsProps
subTreeViewCpt = here.component "subTreeView" cpt
  where
    cpt params@{ action
               , dispatch
               , handed
               , id
               , nodeType
               , session
               , subTreeParams
               } _ =
      do
        let
          SubTreeParams {showtypes} = subTreeParams
        --  (valAction /\ setAction)  = action
        -- _ <- pure $ setAction (const $ setTreeOut valAction Nothing)

        useLoader session (loadSubTree showtypes) $
          \tree ->
            subTreeViewLoaded { action
                              , dispatch
                              , handed
                              , id
                              , nodeType
                              , session
                              , subTreeParams
                              , tree
                              }

loadSubTree :: Array GT.NodeType -> Session -> Aff FTree
loadSubTree nodetypes session = getSubTree session treeId nodetypes
  where
    Session { treeId } = session

getSubTree :: Session -> Int -> Array GT.NodeType -> Aff FTree
getSubTree session treeId showtypes = get session $ GR.NodeAPI GT.Tree (Just treeId) nodeTypes
  where
    nodeTypes     = A.foldl (\a b -> a <> "type=" <> show b <> "&") "?" showtypes

------------------------------------------------------------------------
type CorpusTreeProps =
  ( tree         :: FTree
  | SubTreeParamsProps
  )

subTreeViewLoaded :: Record CorpusTreeProps -> R.Element
subTreeViewLoaded props = R.createElement subTreeViewLoadedCpt props []

subTreeViewLoadedCpt :: R.Component CorpusTreeProps
subTreeViewLoadedCpt = here.component "subTreeViewLoadedCpt" cpt
  where
    cpt p@{dispatch, id, nodeType, session, tree, handed} _ = do
      pure $ H.div {className:"tree"}
                   [H.div { className: if handed == GT.RightHanded
                                         then "righthanded"
                                         else "lefthanded"
                          }
                          [ subTreeTreeView p ]
                   ]

subTreeTreeView :: Record CorpusTreeProps -> R.Element
subTreeTreeView props = R.createElement subTreeTreeViewCpt props []

subTreeTreeViewCpt :: R.Component CorpusTreeProps
subTreeTreeViewCpt = here.component "subTreeTreeViewCpt" cpt where
  cpt p@{ tree: NTree (LNode { id: targetId, name, nodeType }) ary
        , id, subTreeParams, dispatch, action, handed } _ = do
    pure $ H.div {} $ GT.reverseHanded
      [ H.div { className: nodeClass validNodeType }
        [ H.span { className: "text"
                 , on: { click } }
          [ nodeText { handed
                     , isSelected: isSelected targetId valAction
                     , name: " " <> name } []
          , H.span { className: "children" } children
          ]
        ]
      ]
      handed
    where
      nodeClass vnt = "node " <> GT.fldr nodeType true <> " " <> validNodeTypeClass where
        validNodeTypeClass = if vnt then "node-type-valid" else ""
      SubTreeParams { valitypes } = subTreeParams
      sortedAry = A.sortWith (\(NTree (LNode {id:id'}) _) -> id')
        $ A.filter (\(NTree (LNode {id:id'}) _) -> id'/= id) ary
      children = map (\ctree -> subTreeTreeView (p { tree = ctree })) sortedAry
      validNodeType = (A.elem nodeType valitypes) && (id /= targetId)
      clickable    = if validNodeType then "clickable" else ""
      (valAction /\ setAction) = action
      isSelected n action' = case (subTreeOut action') of
        Nothing                   -> false
        (Just (SubTreeOut {out})) -> n == out
      click e = do
        let action' = if not validNodeType then Nothing else Just $ SubTreeOut { in: id, out: targetId }
        E.preventDefault  e
        E.stopPropagation e
        setAction $ const $ setTreeOut valAction action'
