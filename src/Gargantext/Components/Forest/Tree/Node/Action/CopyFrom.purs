module Gargantext.Components.Forest.Tree.Node.Action.CopyFrom where

import DOM.Simple.Console (log2)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Gargantext.Components.Forest.Tree.Node.Action (Props)
import Gargantext.Components.Forest.Tree.Node.FTree (FTree, LNode(..), NTree(..))
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Prelude (discard, map, pure, show, unit, ($), (&&), (/=), (<>))
import Gargantext.Routes as GR
import Gargantext.Sessions (Session(..), get)
import Gargantext.Types as GT
import Reactix as R
import Reactix.DOM.HTML as H


loadNode :: Session -> GT.ID -> Aff FTree
loadNode session nodeId = get session $ GR.NodeAPI GT.Tree (Just nodeId) ""

copyFromCorpusView :: Record Props -> R.Element
copyFromCorpusView props = R.createElement copyFromCorpusViewCpt props []

copyFromCorpusViewCpt :: R.Component Props
copyFromCorpusViewCpt = R.hooksComponent "G.C.F.T.N.A.U.copyFromCorpusView" cpt
  where
    cpt {dispatch, id, nodeType, session} _ = do
      useLoader session loadCorporaTree $
        \tree ->
          copyFromCorpusViewLoaded {dispatch, id, nodeType, session, tree}

type CorpusTreeProps =
  ( tree :: FTree
  | Props
  )

copyFromCorpusViewLoaded :: Record CorpusTreeProps -> R.Element
copyFromCorpusViewLoaded props = R.createElement copyFromCorpusViewLoadedCpt props []

copyFromCorpusViewLoadedCpt :: R.Component CorpusTreeProps
copyFromCorpusViewLoadedCpt = R.hooksComponent "G.C.F.T.N.A.U.copyFromCorpusViewLoadedCpt" cpt
  where
    cpt p@{dispatch, id, nodeType, session, tree} _ = do
      pure $ H.div { className: "copy-from-corpus" } [
        H.div { className: "tree" } [copyFromCorpusTreeView p]
      ]

copyFromCorpusTreeView :: Record CorpusTreeProps -> R.Element
copyFromCorpusTreeView props = R.createElement copyFromCorpusTreeViewCpt props []

copyFromCorpusTreeViewCpt :: R.Component CorpusTreeProps
copyFromCorpusTreeViewCpt = R.hooksComponent "G.C.F.T.N.A.U.copyFromCorpusTreeViewCpt" cpt
  where
    cpt p@{id, tree: NTree (LNode { id: sourceId, name, nodeType }) ary} _ = do
      pure $ {- H.div {} [ H.h5 { className: GT.fldr nodeType true} []
      , -} H.div { className: "node" } ([ H.span { className: "name " <> clickable
                                                              , on: { click: onClick }
                                                              } [ H.text name ]

                                                     ] <> children)
                      -- ]
      where
        children = map (\c -> copyFromCorpusTreeView (p { tree = c })) ary
        validNodeType = (A.elem nodeType [GT.NodeList]) && (id /= sourceId)
        clickable = if validNodeType then "clickable" else ""
        onClick _ = case validNodeType of
          false -> pure unit
          true  -> do
            log2 "[copyFromCorpusTreeViewCpt] issue copy into" id
            log2 "[copyFromCorpusTreeViewCpt] issue copy from" sourceId

loadCorporaTree :: Session -> Aff FTree
loadCorporaTree session = getCorporaTree session treeId
  where
    Session { treeId } = session

getCorporaTree :: Session -> Int -> Aff FTree
getCorporaTree session treeId = get session $ GR.NodeAPI GT.Tree (Just treeId) nodeTypes
  where
    nodeTypes = A.foldl (\a b -> a <> "type=" <> show b <> "&") "?" [ GT.FolderPrivate
                                                             , GT.FolderShared
                                                             , GT.Team
                                                             , GT.FolderPublic
                                                             , GT.Folder
                                                             , GT.Corpus
                                                             , GT.NodeList]
