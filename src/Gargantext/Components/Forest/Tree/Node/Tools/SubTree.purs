module Gargantext.Components.Forest.Tree.Node.Tools.SubTree where

import DOM.Simple.Console (log2)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Effect.Uncurried (mkEffectFn1)
import Effect.Aff (Aff, launchAff)
import Gargantext.Components.Forest.Tree.Node.Action (Props, Action(..))
import Gargantext.Components.Forest.Tree.Node.Settings (SubTreeParams(..))
import Gargantext.Components.Forest.Tree.Node.Tools.FTree (FTree, LNode(..), NTree(..))
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Prelude (discard, map, pure, show, unit, ($), (&&), (/=), (<>), class Eq, const)
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
data SubTreeOut = SubTreeOut { in  :: GT.ID
                             , out :: GT.ID
                             }
------------------------------------------------------------------------
type SubTreeParamsProps =
  ( subTreeOut    :: R.State (Maybe SubTreeOut)
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
               , subTreeOut
               } _ =
      do
        let SubTreeParams {showtypes} = subTreeParams

        useLoader session (loadSubTree showtypes) $
          \tree ->
            subTreeViewLoaded { dispatch
                              , id
                              , nodeType
                              , session
                              , tree
                              , subTreeParams
                              , subTreeOut
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
      pure $ H.div { className: "copy-from-corpus" }
                   [ H.div { className: "tree" }
                           [subTreeTreeView p]
                   ]

subTreeTreeView :: Record CorpusTreeProps -> R.Element
subTreeTreeView props = R.createElement subTreeTreeViewCpt props []

subTreeTreeViewCpt :: R.Component CorpusTreeProps
subTreeTreeViewCpt = R.hooksComponent "G.C.F.T.N.A.U.subTreeTreeViewCpt" cpt
  where
    cpt p@{ id
          , tree: NTree (LNode { id: sourceId
                               , name
                               , nodeType
                               }
                        ) ary
          , subTreeParams
          , dispatch
          , subTreeOut
          } _ = do
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

        children = map (\c -> subTreeTreeView (p { tree = c })) ary

        validNodeType = (A.elem nodeType valitypes) && (id /= sourceId)

        clickable = if validNodeType then "clickable" else ""
        sbto@( subTreeOutParams /\ setSubTreeOut) = subTreeOut
        onClick _ = mkEffectFn1 $ \_ -> case validNodeType of
                         false -> setSubTreeOut (const Nothing)
                         true  -> setSubTreeOut (const $ Just $ SubTreeOut { in: id, out:sourceId})

--------------------------------------------------------------------------------------------

