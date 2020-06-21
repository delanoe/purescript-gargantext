module Gargantext.Components.Forest.Tree.Node.Action.CopyFrom where

import DOM.Simple.Console (log2)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Effect.Aff (Aff)
import Gargantext.Components.Forest.Tree.Node.Action (Props)
import Gargantext.Components.Forest.Tree.Node.Settings (SubTreeParams(..))
import Gargantext.Components.Forest.Tree.Node.Tools.FTree (FTree, LNode(..), NTree(..))
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Prelude (discard, map, pure, show, unit, ($), (&&), (/=), (<>), class Eq)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session(..), get)
import Gargantext.Types as GT
import Reactix as R
import Reactix.DOM.HTML as H


------------------------------------------------------------------------
getNodeTree :: Session -> GT.ID -> Aff FTree
getNodeTree session nodeId = get session $ GR.NodeAPI GT.Tree (Just nodeId) ""

------------------------------------------------------------------------

type SubTreeParamsProps =
  ( subTreeParams :: SubTreeParams
  | Props
  )


copyFromCorpusView :: Record SubTreeParamsProps -> R.Element
copyFromCorpusView props = R.createElement copyFromCorpusViewCpt props []

copyFromCorpusViewCpt :: R.Component SubTreeParamsProps
copyFromCorpusViewCpt = R.hooksComponent "G.C.F.T.N.A.U.copyFromCorpusView" cpt
  where
    cpt params@{ dispatch
        , id
        , nodeType
        , session
        , subTreeParams
        } _ =
      do
        let SubTreeParams {showtypes} = subTreeParams

        useLoader session (loadSubTree showtypes) $
          \tree ->
            copyFromCorpusViewLoaded { dispatch
                                     , id
                                     , nodeType
                                     , session
                                     , tree
                                     , subTreeParams
                                     }

------------------------------------------------------------------------

type CorpusTreeProps =
  ( tree :: FTree
  | SubTreeParamsProps
  )

copyFromCorpusViewLoaded :: Record CorpusTreeProps -> R.Element
copyFromCorpusViewLoaded props = R.createElement copyFromCorpusViewLoadedCpt props []

copyFromCorpusViewLoadedCpt :: R.Component CorpusTreeProps
copyFromCorpusViewLoadedCpt = R.hooksComponent "G.C.F.T.N.A.U.copyFromCorpusViewLoadedCpt" cpt
  where
    cpt p@{dispatch, id, nodeType, session, tree} _ = do
      pure $ H.div { className: "copy-from-corpus" }
                   [ H.div { className: "tree" }
                           [copyFromCorpusTreeView p]
                   ]

copyFromCorpusTreeView :: Record CorpusTreeProps -> R.Element
copyFromCorpusTreeView props = R.createElement copyFromCorpusTreeViewCpt props []

copyFromCorpusTreeViewCpt :: R.Component CorpusTreeProps
copyFromCorpusTreeViewCpt = R.hooksComponent "G.C.F.T.N.A.U.copyFromCorpusTreeViewCpt" cpt
  where
    cpt p@{id, tree: NTree (LNode { id: sourceId, name, nodeType }) ary, subTreeParams} _ = do
      pure $ {- H.div {} [ H.h5 { className: GT.fldr nodeType true} []
      , -} H.div { className: "node" } 
                 ( [ H.span { className: "name " <> clickable
                            , on: { click: onClick }
                            } [ H.text name ]

                   ] <> children
                 )
                      -- ]
      where
        SubTreeParams { valitypes } = subTreeParams
        children = map (\c -> copyFromCorpusTreeView (p { tree = c })) ary
        validNodeType = (A.elem nodeType valitypes) && (id /= sourceId)
        clickable = if validNodeType then "clickable" else ""
        onClick _ = case validNodeType of
          false -> pure unit
          true  -> do
            log2 "[copyFromCorpusTreeViewCpt] issue copy into" id
            log2 "[copyFromCorpusTreeViewCpt] issue copy from" sourceId

--------------------------------------------------------------------------------------------
loadSubTree :: Array GT.NodeType -> Session -> Aff FTree
loadSubTree nodetypes session = getSubTree session treeId nodetypes
  where
    Session { treeId } = session

getSubTree :: Session -> Int -> Array GT.NodeType -> Aff FTree
getSubTree session treeId showtypes = get session $ GR.NodeAPI GT.Tree (Just treeId) nodeTypes
  where
    nodeTypes     = A.foldl (\a b -> a <> "type=" <> show b <> "&") "?" showtypes


