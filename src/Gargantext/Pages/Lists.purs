module Gargantext.Pages.Lists where

import Data.Array (head)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, throwError)
import Effect.Exception (error)
import Reactix as R
import Thermite (Spec)
--------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Components.Node (NodePoly(..), HyperdataList)
import Gargantext.Components.Loader2 (useLoader)
import Gargantext.Components.Table as Table
import Gargantext.Config      (toUrl, Path(..), NodeType(..), End(..))
import Gargantext.Config.REST (get)
import Gargantext.Pages.Lists.Tabs.Types (CorpusData, CorpusInfo(..))
import Gargantext.Pages.Lists.Tabs.Specs (elt) as Tabs
import Gargantext.Utils.Reactix as R2

------------------------------------------------------------------------
layout :: Spec {} {nodeId :: Int} Void
layout =
  R2.elSpec $ R.hooksComponent "ListsLoader" \{nodeId} _ ->
    useLoader nodeId getCorpus $ \{loaded: corpusData} ->
      let {corpusId
          ,corpusNode:
            NodePoly { name: title
                     , date: date'
                     , hyperdata: CorpusInfo corpus
                     }
          } = corpusData in
      R2.toElement $
        Table.renderTableHeaderLayout
          { title: "Corpus " <> title
          , desc:  corpus.desc
          , query: corpus.query
          , date:  date'
          , user:  corpus.authors
          }
        <> [R2.buff $ Tabs.elt {corpusId, corpusData}]
------------------------------------------------------------------------

getCorpus :: Int -> Aff CorpusData
getCorpus listId = do
  -- fetch corpus via lists parentId
  (NodePoly {parentId: corpusId} :: NodePoly {})       <- get $ toUrl Back Corpus $ Just listId
  corpusNode     <- get $ toUrl Back Corpus $ Just corpusId
  defaultListIds <- get $ toUrl Back (Children NodeList 0 1 Nothing) $ Just corpusId
  case (head defaultListIds :: Maybe (NodePoly HyperdataList)) of
    Just (NodePoly { id: defaultListId }) ->
      pure {corpusId, corpusNode, defaultListId}
    Nothing ->
      throwError $ error "Missing default list"
