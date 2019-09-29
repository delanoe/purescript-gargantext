module Gargantext.Pages.Lists where

import Prelude ((<<<))
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, throwError)
import Effect.Exception (error)
import Reactix as R
--------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Components.Node (NodePoly(..), HyperdataList)
import Gargantext.Components.Table as Table
import Gargantext.Config.REST (get)
import Gargantext.Ends (url)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Pages.Lists.Tabs as Tabs
import Gargantext.Routes (SessionRoute(NodeAPI, Children))
import Gargantext.Sessions (Session)
import Gargantext.Types (NodeType(..))

------------------------------------------------------------------------

type Props = ( nodeId :: Int, session :: Session )

listsLayout :: Record Props -> R.Element
listsLayout props = R.createElement listsLayoutCpt props []

listsLayoutCpt :: R.Component Props
listsLayoutCpt = R.hooksComponent "G.P.Lists.listsLayout" cpt
  where
    cpt {nodeId, session} _ =
      useLoader nodeId (getCorpus session) $
        \corpusData@{corpusId, defaultListId, corpusNode: NodePoly poly} ->
          let { name, date, hyperdata: Tabs.CorpusInfo corpus } = poly
              { desc, query, authors: user } = corpus in
          R.fragment
          [ Table.tableHeaderLayout
            { title: "Corpus " <> name, desc, query, user, date }
         , Tabs.tabs {session, corpusId, corpusData}]
------------------------------------------------------------------------

getCorpus :: Session -> Int -> Aff Tabs.CorpusData
getCorpus session listId = do
  -- fetch corpus via lists parentId
  (NodePoly {parentId: corpusId} :: NodePoly {}) <- get nodePolyUrl
  corpusNode     <- get $ corpusNodeUrl corpusId
  defaultListIds <- get $ defaultListIdsUrl corpusId
  case (head defaultListIds :: Maybe (NodePoly HyperdataList)) of
    Just (NodePoly { id: defaultListId }) ->
      pure {corpusId, corpusNode, defaultListId}
    Nothing ->
      throwError $ error "Missing default list"
  where
    nodePolyUrl = url session (NodeAPI Corpus (Just listId))
    corpusNodeUrl = url session <<< NodeAPI Corpus <<< Just
    defaultListIdsUrl = url session <<< Children NodeList 0 1 Nothing <<< Just
