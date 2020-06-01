module Gargantext.Components.Forest.Tree.Node.Action.CopyFrom where

import Data.Array as A
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log2)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import React.SyntheticEvent as E
import Reactix as R
import Reactix.DOM.HTML as H
import URI.Extra.QueryPairs as QP
import Web.File.FileReader.Aff (readAsText)

import Gargantext.Prelude (class Show, Unit, bind, const, discard, map, pure, show, unit, void, ($), (&&), (/=), (<>))

import Gargantext.Components.Lang (readLang, Lang(..))
import Gargantext.Components.Forest.Tree.Node.Action (Action(..))
import Gargantext.Components.Forest.Tree.Node.FTree (FTree, ID, LNode(..), NTree(..))
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session(..), postWwwUrlencoded, get)
import Gargantext.Types as GT
import Gargantext.Utils (id)
import Gargantext.Utils.Reactix as R2


loadNode :: Session -> GT.ID -> Aff FTree
loadNode session nodeId = get session $ NodeAPI GT.Tree (Just nodeId) ""

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
