module Gargantext.Pages.Texts where

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
import Gargantext.Config
import Gargantext.Config.REST (get)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Pages.Texts.Tabs (CorpusData, CorpusInfo(..))
import Gargantext.Pages.Texts.Tabs as Tabs
import Gargantext.Utils.Reactix as R2

type Props = ( ends :: Ends, nodeId :: Int )

textsLayout :: Record Props -> R.Element
textsLayout props = R.createElement textsLayoutCpt props []

------------------------------------------------------------------------
textsLayoutCpt :: R.Component Props
textsLayoutCpt = R.hooksComponent "TextsLoader" cpt
  where
    cpt {nodeId,ends} _ =
      useLoader nodeId (getCorpus ends) $
        \corpusData@{corpusId, corpusNode, defaultListId} ->
          let
            NodePoly { name, date, hyperdata: CorpusInfo corpus } = corpusNode
            {desc, query, authors: user} = corpus
            tabs = Tabs.tabs {ends, corpusId, corpusData}
            title = "Corpus " <> name
            headerProps = { title, desc, query, date, user } in
          R.fragment [Table.tableHeaderLayout headerProps, tabs]

          

------------------------------------------------------------------------

getCorpus :: Ends -> Int -> Aff CorpusData
getCorpus ends textsId = do
  -- fetch corpus via texts parentId
  (NodePoly {parentId: corpusId} :: NodePoly {}) <- get nodePolyUrl
  corpusNode     <- get $ corpusNodeUrl corpusId
  defaultListIds <- get $ defaultListIdsUrl corpusId
  case (head defaultListIds :: Maybe (NodePoly HyperdataList)) of
    Just (NodePoly { id: defaultListId }) ->
      pure {corpusId, corpusNode, defaultListId}
    Nothing ->
      throwError $ error "Missing default list"
  where
    nodePolyUrl = url ends $ NodeAPI NodeList (Just textsId)
    corpusNodeUrl = url ends <<< NodeAPI Corpus <<< Just
    defaultListIdsUrl = url ends <<< Children NodeList 0 1 Nothing <<< Just
