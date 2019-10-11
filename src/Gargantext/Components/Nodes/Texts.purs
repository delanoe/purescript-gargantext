module Gargantext.Components.Nodes.Texts where

import Prelude ((<<<))
import Data.Array (head)
import Data.Maybe (Maybe(..))
import DOM.Simple.Console (log2)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, throwError)
import Effect.Exception (error)
import Reactix as R
--------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Components.Loader (loader)
import Gargantext.Components.Node (NodePoly(..), HyperdataList)
import Gargantext.Components.Table as Table
import Gargantext.Config.REST (get)
import Gargantext.Ends (url)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Components.Nodes.Texts.Tabs (CorpusData, CorpusInfo(..))
import Gargantext.Components.Nodes.Texts.Tabs as Tabs
import Gargantext.Routes (SessionRoute(NodeAPI, Children))
import Gargantext.Sessions (Session)
import Gargantext.Types (NodeType(..))

type Props = ( session :: Session, nodeId :: Int )

textsLayout :: Record Props -> R.Element
textsLayout props = R.createElement textsLayoutCpt props []

------------------------------------------------------------------------
textsLayoutCpt :: R.Component Props
textsLayoutCpt = R.hooksComponent "G.P.Texts.textsLayout" cpt where
  cpt path@{session} _ = do
    pure $ loader path loadCorpus paint
    where
      paint corpusData@{corpusId, corpusNode, defaultListId} =
        R.fragment [ Table.tableHeaderLayout headerProps, tabs ]
        where
          NodePoly { name, date, hyperdata: CorpusInfo corpus } = corpusNode
          {desc, query, authors: user} = corpus
          tabs = Tabs.tabs {session, corpusId, corpusData}
          title = "Corpus " <> name
          headerProps = { title, desc, query, date, user }
------------------------------------------------------------------------

loadCorpus :: Record Props -> Aff CorpusData
loadCorpus {session, nodeId} = do
  liftEffect $ log2 "nodepolyurl: " nodePolyUrl
  -- fetch corpus via texts parentId
  (NodePoly {parentId: corpusId} :: NodePoly {}) <- get nodePolyUrl
  liftEffect $ log2 "corpusnodeurl: " $ corpusNodeUrl corpusId
  corpusNode     <- get $ corpusNodeUrl corpusId
  liftEffect $ log2 "defaultlistidsurl: " $ defaultListIdsUrl corpusId
  defaultListIds <- get $ defaultListIdsUrl corpusId
  case (head defaultListIds :: Maybe (NodePoly HyperdataList)) of
    Just (NodePoly { id: defaultListId }) ->
      pure {corpusId, corpusNode, defaultListId}
    Nothing ->
      throwError $ error "Missing default list"
  where
    nodePolyUrl = url session $ NodeAPI Corpus (Just nodeId)
    corpusNodeUrl = url session <<< NodeAPI Corpus <<< Just
    defaultListIdsUrl = url session <<< Children NodeList 0 1 Nothing <<< Just
