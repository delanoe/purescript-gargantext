module Gargantext.Components.Document.Types where

import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..))
import Simple.JSON as JSON

import Gargantext.Prelude

import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Core.NgramsTable.Types (CoreState, Versioned(..) , VersionedNgramsTable)
import Gargantext.Sessions (Session)
import Gargantext.Types (ListId, NodeID, TabType)

type DocPath = {
    listIds   :: Array ListId
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

derive instance Generic Status _
derive instance Newtype Status _
derive newtype instance JSON.ReadForeign Status
derive newtype instance JSON.WriteForeign Status
instance Show Status where show = genericShow

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

derive instance Generic DocumentV3 _
derive instance Newtype DocumentV3 _
derive newtype instance JSON.ReadForeign DocumentV3
derive newtype instance JSON.WriteForeign DocumentV3
instance Show DocumentV3 where show = genericShow

defaultNodeDocumentV3 :: NodePoly DocumentV3
defaultNodeDocumentV3 =
  NodePoly { id : 0
           , typename : 0
           , userId   : 0
           , parentId : Just 0
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

newtype Document =
  Document
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

derive instance Generic Document _
derive instance Newtype Document _
derive newtype instance JSON.ReadForeign Document
derive newtype instance JSON.WriteForeign Document
instance Eq Document where eq = genericEq
instance Show Document where show = genericShow

defaultNodeDocument :: NodeDocument
defaultNodeDocument =
  NodePoly { id : 0
           , typename : 0
           , userId   : 0
           , parentId : Just 0
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
