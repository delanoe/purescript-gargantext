module Gargantext.Components.Nodes.Corpus.Document.Types where

import Data.Argonaut (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))

import Gargantext.Prelude

import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.NgramsTable.Core (CoreState, Versioned(..) , VersionedNgramsTable)
import Gargantext.Sessions (Session)
import Gargantext.Types (ListId, NodeID, TabType)

type DocPath =
  { listIds   :: Array ListId
  , mCorpusId :: Maybe NodeID
  , nodeId    :: NodeID
  , session   :: Session
  , tabType   :: TabType
  }

type NodeDocument = NodePoly Document

type LoadedData =
  { document    :: NodeDocument
  , ngramsTable :: VersionedNgramsTable
  }

type Props = (
    loaded         :: LoadedData
  , path           :: DocPath
  )

-- This is a subpart of NgramsTable.State.
type State = CoreState ()

initialState
  :: forall props others
  .  { loaded :: { ngramsTable :: VersionedNgramsTable | others }
     | props }
  -> State
initialState {loaded: {ngramsTable: Versioned {version}}} =
  { ngramsLocalPatch: mempty
  , ngramsStagePatch: mempty
  , ngramsValidPatch: mempty
  , ngramsVersion:    version
  }

newtype Status = Status { failed    :: Int
                        , succeeded :: Int
                        , remaining :: Int
                        }

newtype DocumentV3 =
  DocumentV3 { abstract           :: Maybe String
             , authors            :: Maybe String
             --, error              :: Maybe String
             , language_iso2      :: Maybe String
             , language_iso3      :: Maybe String
             , language_name      :: Maybe String
             , publication_date   :: Maybe String
             , publication_day    :: Maybe Int
             , publication_hour   :: Maybe Int
             , publication_minute :: Maybe Int
             , publication_month  :: Maybe Int
             , publication_second :: Maybe Int
             , publication_year   :: Maybe Int
             , realdate_full_     :: Maybe String
             , source             :: Maybe String
             , statuses           :: Maybe (Array Status)
             , title              :: Maybe String
             }

defaultNodeDocumentV3 :: NodePoly DocumentV3
defaultNodeDocumentV3 =
  NodePoly { id : 0
           , typename : 0
           , userId   : 0
           , parentId : 0
           , name     : "Default name"
           , date     : "Default date"
           , hyperdata : defaultDocumentV3
         }

defaultDocumentV3 :: DocumentV3
defaultDocumentV3 =
  DocumentV3 { abstract           : Nothing
             , authors            : Nothing
             --, error              : Nothing
             , language_iso2      : Nothing
             , language_iso3      : Nothing
             , language_name      : Nothing
             , publication_date   : Nothing
             , publication_day    : Nothing
             , publication_hour   : Nothing
             , publication_minute : Nothing
             , publication_month  : Nothing
             , publication_second : Nothing
             , publication_year   : Nothing
             , realdate_full_     : Nothing
             , source             : Nothing
             , statuses           : Nothing
             , title              : Nothing
             }

data Document
  = Document
    { abstract           :: Maybe String
    , authors            :: Maybe String
    , bdd                :: Maybe String
    , doi                :: Maybe String
    , language_iso2      :: Maybe String
    -- , page               :: Maybe Int
    , publication_date   :: Maybe String
    --, publication_second :: Maybe Int
    --, publication_minute :: Maybe Int
    --, publication_hour   :: Maybe Int
    , publication_day    :: Maybe Int
    , publication_month  :: Maybe Int
    , publication_year   :: Maybe Int
    , source             :: Maybe String
    , institutes         :: Maybe String
    , title              :: Maybe String
    , uniqId             :: Maybe String
    --, url                :: Maybe String
    --, text               :: Maybe String
    }


defaultNodeDocument :: NodeDocument
defaultNodeDocument =
  NodePoly { id : 0
           , typename : 0
           , userId   : 0
           , parentId : 0
           , name     : "Default name"
           , date     : "Default date"
           , hyperdata : defaultDocument
         }

-- TODO: BUG if DOI does not exist, page is not shown
defaultDocument :: Document
defaultDocument =
  Document { abstract           : Nothing
           , authors            : Nothing
           , bdd                : Nothing
           , doi                : Nothing
           , language_iso2      : Nothing
           --, page               : Nothing
           , publication_date   : Nothing
           --, publication_second : Nothing
           --, publication_minute : Nothing
           --, publication_hour   : Nothing
           , publication_day    : Nothing
           , publication_month  : Nothing
           , publication_year   : Nothing
           , source             : Nothing
           , institutes         : Nothing
           , title              : Nothing
           , uniqId             : Nothing
           --, url                : Nothing
           --, text               : Nothing
           }

derive instance genericDocument   :: Generic Document   _
derive instance genericDocumentV3 :: Generic DocumentV3 _
derive instance genericStatus     :: Generic Status     _

instance showDocument :: Show Document where
  show = genericShow

instance showDocumentV3 :: Show DocumentV3 where
  show = genericShow

instance showStatus :: Show Status where
  show = genericShow

instance decodeStatus :: DecodeJson Status
  where
    decodeJson json = do
      obj <- decodeJson json
      failed <- obj .: "failed"
      succeeded <- obj .: "succeeded"
      remaining <- obj .: "remaining"
      pure $ Status {failed, succeeded, remaining}


instance decodeDocumentV3 :: DecodeJson DocumentV3
  where
    decodeJson json = do
      obj <- decodeJson json
      abstract <- obj .:? "abstract"
      authors  <- obj .: "authors"
      --error    <- obj .: "error"
      language_iso2 <- obj .: "language_iso2"
      language_iso3 <- obj .: "language_iso3"
      language_name <- obj .: "language_name"
      publication_date   <- obj .: "publication_date"
      publication_day    <- obj .: "publication_day"
      publication_hour   <- obj .: "publication_hour"
      publication_minute <- obj .: "publication_minute"
      publication_month  <- obj .: "publication_month"
      publication_second <- obj .: "publication_second"
      publication_year   <- obj .: "publication_year"
      realdate_full_     <- obj .: "realdate_full_"
      source   <- obj .: "source"
      statuses <- obj .: "statuses"
      title    <- obj .: "title"
      pure $ DocumentV3 { abstract
                        , authors
                        --, error
                        , language_iso2
                        , language_iso3
                        , language_name
                        , publication_date
                        , publication_day
                        , publication_hour
                        , publication_minute
                        , publication_month
                        , publication_second
                        , publication_year
                        , realdate_full_
                        , source
                        , statuses
                        , title
                        }

instance decodeDocument :: DecodeJson Document
  where
    decodeJson json = do
      obj <- decodeJson json
      abstract <- obj .:? "abstract"
      authors  <- obj .:? "authors"
      bdd      <- obj .:? "bdd"
      doi      <- obj .:? "doi"
      language_iso2 <- obj .:? "language_iso2"
      -- page          <- obj .:? "page"
      publication_date   <- obj .:? "publication_date"
      --publication_second <- obj .:? "publication_second"
      --publication_minute <- obj .:? "publication_minute"
      --publication_hour   <- obj .:? "publication_hour"
      publication_day    <- obj .:? "publication_day"
      publication_month  <- obj .:? "publication_month"
      publication_year   <- obj .:? "publication_year"
      source             <- obj .:? "sources"
      institutes         <- obj .:? "institutes"
      title              <- obj .:? "title"
      uniqId             <- obj .:? "uniqId"
      --url                <- obj .: "url"
      --text               <- obj .: "text"
      pure $ Document { abstract
                      , authors
                      , bdd
                      , doi
                      , language_iso2
                      -- , page
                      , publication_date
                      --, publication_second
                      --, publication_minute
                      --, publication_hour
                      , publication_day
                      , publication_month
                      , publication_year
                      , source
                      , institutes
                      , title
                      , uniqId
                      --, url
                      --, text
                      }
