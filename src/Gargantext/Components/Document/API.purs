module Gargantext.Components.Document.API
  ( loadData
  ) where

import Gargantext.Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Gargantext.Components.Document.Types (DocPath, LoadedData, NodeDocument)
import Gargantext.Core.NgramsTable.Functions (loadNgramsTable)
import Gargantext.Components.Search (SearchType(..))
import Gargantext.Config.REST (AffRESTError)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, get)
import Gargantext.Types (NodeType(..), ScoreType(..))
loadDocument :: Session -> Int -> AffRESTError NodeDocument
loadDocument session nodeId = get session $ NodeAPI Context (Just nodeId) ""

loadData :: DocPath -> AffRESTError LoadedData
loadData { listIds, nodeId, session, tabType } = do
  eDocument <- loadDocument session nodeId
  case eDocument of
    Left err -> pure $ Left err
    Right document -> do
      eNgramsTable <- loadNgramsTable
        { listIds
        , nodeId
        , params: { offset : 0, limit : 100, orderBy: Nothing, searchType: SearchDoc}
        , scoreType: Occurrences
        , searchQuery: ""
        , session
        , tabType
        , termListFilter: Nothing
        , termSizeFilter: Nothing
        }
      pure $ (\ngramsTable -> { document, ngramsTable }) <$> eNgramsTable
