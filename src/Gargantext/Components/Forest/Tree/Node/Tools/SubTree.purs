module Gargantext.Components.Forest.Tree.Node.Tools.SubTree where

import Gargantext.Prelude

import Data.Array (length)
import Data.Array as A
import Data.Either (Either)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Gargantext.Components.App.Data (Boxes)
import Gargantext.Components.Forest.Tree.Node.Action (Props, subTreeOut, setTreeOut)
import Gargantext.Components.Forest.Tree.Node.Action.Types (Action)
import Gargantext.Components.Forest.Tree.Node.Tools (nodeText)
import Gargantext.Components.Forest.Tree.Node.Tools.FTree (FTree, LNode(..), NTree(..))
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree.Types (SubTreeParams(..), SubTreeOut(..))
import Gargantext.Config.REST (RESTError, logRESTError)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session(..), get)
import Gargantext.Types as GT
import Gargantext.Utils ((?))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Tools.SubTree"

type SubTreeParamsIn =
  ( boxes         :: Boxes
  , subTreeParams :: SubTreeParams
  | Props
  )

------------------------------------------------------------------------
type SubTreeParamsProps =
  ( action    :: T.Box Action
  | SubTreeParamsIn
  )

subTreeView :: R2.Component SubTreeParamsProps
subTreeView = R.createElement $ R.memo' subTreeViewCpt
subTreeViewCpt :: R.Component SubTreeParamsProps
subTreeViewCpt = here.component "subTreeView" cpt
  where
    cpt { action
        , boxes
        , dispatch
        , id
        , nodeType
        , session
        , subTreeParams
        } _ = do
      let
        SubTreeParams {showtypes} = subTreeParams
      --  (valAction /\ setAction)  = action
      -- _ <- pure $ setAction (const $ setTreeOut valAction Nothing)

      useLoader { errorHandler
                , loader: loadSubTree showtypes
                , path: session
                , render: \tree ->
                    subTreeViewLoaded { action
                                      , boxes
                                      , dispatch
                                      , id
                                      , nodeType
                                      , session
                                      , subTreeParams
                                      , tree
                                      } []  }
      where
        errorHandler = logRESTError here "[subTreeView]"

loadSubTree :: Array GT.NodeType -> Session -> Aff (Either RESTError FTree)
loadSubTree nodetypes session = getSubTree session treeId nodetypes
  where
    Session { treeId } = session

getSubTree :: Session -> Int -> Array GT.NodeType -> Aff (Either RESTError FTree)
getSubTree session treeId showtypes = get session $ GR.NodeAPI GT.Tree (Just treeId) nodeTypes
  where
    nodeTypes     = A.foldl (\a b -> a <> "type=" <> show b <> "&") "?" showtypes

------------------------------------------------------------------------
type CorpusTreeProps =
  ( tree         :: FTree
  | SubTreeParamsProps
  )

subTreeViewLoaded :: R2.Component CorpusTreeProps
subTreeViewLoaded = R.createElement subTreeViewLoadedCpt
subTreeViewLoadedCpt :: R.Component CorpusTreeProps
subTreeViewLoadedCpt = here.component "subTreeViewLoaded" cpt where
  cpt props _ = do

    let pRender = Record.merge { render: subTreeTreeView } props

    pure $

      H.div { className: "subtree" }
      [ subTreeTreeView (CorpusTreeRenderProps pRender) [] ]

newtype CorpusTreeRenderProps = CorpusTreeRenderProps
  { render :: CorpusTreeRenderProps -> Array R.Element -> R.Element
  | CorpusTreeProps
  }

subTreeTreeView :: CorpusTreeRenderProps -> Array R.Element -> R.Element
subTreeTreeView = R2.ntCreateElement subTreeTreeViewCpt
subTreeTreeViewCpt :: R2.NTComponent CorpusTreeRenderProps
subTreeTreeViewCpt = here.ntComponent "subTreeTreeView" cpt where
  cpt (CorpusTreeRenderProps p@{ id
                               , render
                               , subTreeParams
                               , tree: NTree (LNode { id: targetId, name, nodeType }) ary }) _ = do
    -- Hooks
    action <- R2.useLive' p.action
    isExpanded /\ isExpandedBox <- R2.useBox' false
    -- Computed
    let
        expandCbk _ = T.modify_ not isExpandedBox

        selectCbk _ = do
          params <- pure $
            if validNodeType
            then Just $ SubTreeOut { in: id, out: targetId }
            else Nothing
          T.modify_ (\a -> setTreeOut a params) p.action

        children = (map (\ctree -> render (CorpusTreeRenderProps (p { tree = ctree })) []) sortedAry) :: Array R.Element

        hasChild = length children > 0

    -- Render
    pure $

      H.div
      { className: intercalate " "
          [ "subtree__node"
          , validNodeType ? "subtree__node--can-be-selected" $ ""
          ]
      }
      [
          H.div
          { className: "subtree__node__text" }
          [
            H.div
            { className: "subtree__node__icons"
            , on: { click: expandCbk }
            }
            [
              H.span { className: GT.fldr nodeType true } []
            ,
              R2.if' hasChild $

                if isExpanded then
                  H.span { className: "fa fa-chevron-down" } []
                else
                  H.span { className: "fa fa-chevron-right" } []
            ]
          ,
            H.div
            { on: { click: selectCbk } }
            [
              nodeText
              { isSelected: isSelected targetId action
              , name
              }
            ]
          ]
      ,
        R2.if' (hasChild && isExpanded) $
          H.div { className: "subtree__node__children" }
          children
      ]
    where
      SubTreeParams { valitypes } = subTreeParams
      sortedAry = A.sortWith (\(NTree (LNode {id:id'}) _) -> id')
        $ A.filter (\(NTree (LNode {id:id'}) _) -> id'/= id) ary
      validNodeType = (A.elem nodeType valitypes) && (id /= targetId)
      isSelected n action = case (subTreeOut action) of
        Nothing                   -> false
        (Just (SubTreeOut {out})) -> n == out
