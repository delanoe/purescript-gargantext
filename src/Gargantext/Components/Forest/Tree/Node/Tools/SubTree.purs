module Gargantext.Components.Forest.Tree.Node.Tools.SubTree where

import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Uncurried (mkEffectFn1)
import Effect.Aff (Aff)
import Gargantext.Components.Forest.Tree.Node.Action (Props, Action, subTreeOut, setTreeOut)
import Gargantext.Components.Forest.Tree.Node.Tools.SubTree.Types (SubTreeParams(..), SubTreeOut(..))
import Gargantext.Components.Forest.Tree.Node.Tools.FTree (FTree, LNode(..), NTree(..))
import Gargantext.Components.Forest.Tree.Node.Tools (nodeText)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Prelude (map, pure, show, ($), (&&), (/=), (<>), const, (==){-, discard, bind, void-})
import Gargantext.Routes as GR
import Gargantext.Sessions (Session(..), get)
import Gargantext.Types as GT
import Reactix as R
import Reactix.DOM.HTML as H

type SubTreeParamsIn =
  ( subTreeParams :: SubTreeParams
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
subTreeViewCpt = R.hooksComponent "G.C.F.T.N.A.U.subTreeView" cpt
  where
    cpt params@{ dispatch
               , id
               , nodeType
               , session
               , subTreeParams
               , action
               } _ =
      do
        let
          SubTreeParams {showtypes} = subTreeParams
        --  (valAction /\ setAction)  = action
        -- _ <- pure $ setAction (const $ setTreeOut valAction Nothing)

        useLoader session (loadSubTree showtypes) $
          \tree ->
            subTreeViewLoaded { dispatch
                              , id
                              , nodeType
                              , session
                              , tree
                              , subTreeParams
                              , action
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
subTreeViewLoadedCpt = R.hooksComponent "G.C.F.T.N.A.U.subTreeViewLoadedCpt" cpt
  where
    cpt p@{dispatch, id, nodeType, session, tree} _ = do
      pure $ H.div {className:"panel panel-primary"}
                   [H.div { className: "copy-from-corpus" }
                          [ H.div { className: "tree" }
                                  [subTreeTreeView p]
                          ]
                   ]

subTreeTreeView :: Record CorpusTreeProps -> R.Element
subTreeTreeView props = R.createElement subTreeTreeViewCpt props []

subTreeTreeViewCpt :: R.Component CorpusTreeProps
subTreeTreeViewCpt = R.hooksComponent "G.C.F.T.N.A.U.subTreeTreeViewCpt" cpt
  where
    cpt p@{ id
          , tree: NTree (LNode { id: targetId
                               , name
                               , nodeType
                               }
                        ) ary
          , subTreeParams
          , dispatch
          , action
          } _ = do
            pure $ H.div {} [ H.div { className: "node " <> GT.fldr nodeType true} 
                                    ( [ H.span { className: "name " <> clickable
                                               , on: { click: onClick }
                                               } [ nodeText { isSelected: isSelected targetId valAction
                                                            , name: " " <> name
                                                            }
                                                 ]

                                    ] <> children
                                  )
                       ]
      where

        SubTreeParams { valitypes } = subTreeParams
        
        sortedAry = A.sortWith (\(NTree (LNode {id:id'}) _) -> id')
                  $ A.filter (\(NTree (LNode {id:id'}) _) -> id'/= id) ary

        children = map (\ctree -> subTreeTreeView (p { tree = ctree })) sortedAry

        validNodeType = (A.elem nodeType valitypes) && (id /= targetId)

        clickable    = if validNodeType then "clickable" else ""

        ( valAction /\ setAction) = action

        isSelected n action' = case (subTreeOut action') of
            Nothing                   -> false
            (Just (SubTreeOut {out})) -> n == out

        onClick _ = mkEffectFn1 $ \_ -> case validNodeType of
                         false -> setAction (const $ setTreeOut valAction Nothing)
                         true  -> setAction (const $ setTreeOut valAction (Just $ SubTreeOut { in: id, out:targetId}))


--------------------------------------------------------------------------------------------

