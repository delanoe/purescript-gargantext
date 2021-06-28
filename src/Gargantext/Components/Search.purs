module Gargantext.Components.Search where

import Gargantext.Prelude (class Eq, class Show)
import Data.Argonaut as Argonaut
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe)

import Gargantext.Utils.Argonaut (genericSumDecodeJson, genericSumEncodeJson, genericEnumDecodeJson, genericEnumEncodeJson)

-- Example:
--   [["machine","learning"],["artificial","intelligence"]]
-- This searches for documents with "machine learning" or "artificial intelligence"
type TextQuery = Array (Array String)
------------------------------------------------------------------------
data SearchType = SearchDoc | SearchContact

derive instance Eq SearchType
derive instance Generic SearchType _
instance Show SearchType where
  show = genericShow
instance Argonaut.DecodeJson SearchType where
  decodeJson = genericEnumDecodeJson
instance Argonaut.EncodeJson SearchType where
  encodeJson = genericEnumEncodeJson
------------------------------------------------------------------------

data SearchQuery = SearchQuery { query :: Array String, expected :: SearchType }

derive instance Eq SearchQuery
derive instance Generic SearchQuery _
instance Show SearchQuery where
  show = genericShow
instance Argonaut.DecodeJson SearchQuery where
  decodeJson = genericSumDecodeJson
instance Argonaut.EncodeJson SearchQuery where
  encodeJson = genericSumEncodeJson

------------------------------------------------------------------------
------------------------------------------------------------------------
data SearchResult = SearchResult { result :: SearchResultTypes }

derive instance Eq SearchResult
derive instance Generic SearchResult _
instance Show SearchResult where
  show = genericShow
instance Argonaut.DecodeJson SearchResult where
  decodeJson = genericSumDecodeJson
instance Argonaut.EncodeJson SearchResult where
  encodeJson = genericSumEncodeJson


------------------------------------------------------------------------
data SearchResultTypes = SearchResultDoc     { docs     :: Array Document}
                       | SearchNoResult      { message  :: String        }
                       | SearchResultContact { contacts :: Array Contact }

derive instance Eq SearchResultTypes
derive instance Generic SearchResultTypes _
instance Show SearchResultTypes where
  show = genericShow
instance Argonaut.DecodeJson SearchResultTypes where
  decodeJson = genericSumDecodeJson
instance Argonaut.EncodeJson SearchResultTypes where
  encodeJson = genericSumEncodeJson


------------------------------------------------------------------------
data Document =
  Document { id         :: Int
           , created    :: String
           , title      :: String
           , hyperdata  :: HyperdataRowDocument
           , category   :: Int
           , score      :: Int
           }
derive instance Generic Document _
instance Eq Document where
  eq = genericEq
instance Show Document where
  show = genericShow
instance Argonaut.DecodeJson Document where
  decodeJson = genericSumDecodeJson
instance Argonaut.EncodeJson Document where
  encodeJson = genericSumEncodeJson

------------------------------------------------------------------------
newtype HyperdataRowDocument =
  HyperdataRowDocument { bdd                :: Maybe String
                       , doi                :: Maybe String
                       , url                :: Maybe String
                       , uniqId             :: Maybe String
                       , uniqIdBdd          :: Maybe String
                       , page               :: Maybe Int
                       , title              :: Maybe String
                       , authors            :: Maybe String
                       , institutes         :: Maybe String
                       , source             :: Maybe String
                       , abstract           :: Maybe String
                       , publication_date   :: Maybe String
                       , publication_year   :: Maybe Int
                       , publication_month  :: Maybe Int
                       , publication_day    :: Maybe Int
                       , publication_hour   :: Maybe Int
                       , publication_minute :: Maybe Int
                       , publication_second :: Maybe Int
                       , language_iso2      :: Maybe String
                       }

derive instance Eq HyperdataRowDocument
derive instance Generic HyperdataRowDocument _
instance Show HyperdataRowDocument where
  show = genericShow
instance Argonaut.DecodeJson HyperdataRowDocument where
  decodeJson = genericSumDecodeJson
instance Argonaut.EncodeJson HyperdataRowDocument where
  encodeJson = genericSumEncodeJson

------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
data Contact =
  Contact  { c_id         :: Int
           , c_created    :: String
           , c_hyperdata  :: HyperdataRowContact
           , c_score      :: Int
           , c_annuaireId :: Int
           }

derive instance Eq Contact
derive instance Generic Contact _
instance Show Contact where
  show = genericShow
instance Argonaut.DecodeJson Contact where
  decodeJson = genericSumDecodeJson
instance Argonaut.EncodeJson Contact where
  encodeJson = genericSumEncodeJson


data HyperdataRowContact =
     HyperdataRowContact { firstname    :: String
                         , lastname   :: String
                         , labs       :: String
                         }
derive instance Eq HyperdataRowContact
derive instance Generic HyperdataRowContact _
instance Show HyperdataRowContact where
  show = genericShow
instance Argonaut.DecodeJson HyperdataRowContact where
  decodeJson = genericSumDecodeJson
instance Argonaut.EncodeJson HyperdataRowContact where
  encodeJson = genericSumEncodeJson




data HyperdataContact =
     HyperdataContact { bdd    :: Maybe String
                      , who    :: Maybe ContactWho
                      , "where" :: Array ContactWhere
                      , title   :: Maybe String
                      , source         :: Maybe String
                      , lastValidation :: Maybe String
                      , uniqIdBdd      :: Maybe String
                      , uniqId         :: Maybe String
                      }
derive instance Eq HyperdataContact
derive instance Generic HyperdataContact _
instance Show HyperdataContact where
  show = genericShow
instance Argonaut.DecodeJson HyperdataContact where
  decodeJson = genericSumDecodeJson
instance Argonaut.EncodeJson HyperdataContact where
  encodeJson = genericSumEncodeJson

-------
data ContactWho =
     ContactWho { id          :: Maybe String
                , firstName   :: Maybe String
                , lastName    :: Maybe String
                , keywords :: Array String
                , freetags :: Array String
                }
derive instance Eq ContactWho
derive instance Generic ContactWho _
instance Show ContactWho where
  show = genericShow
instance Argonaut.DecodeJson ContactWho where
  decodeJson = genericSumDecodeJson
instance Argonaut.EncodeJson ContactWho where
  encodeJson = genericSumEncodeJson


data ContactWhere =
     ContactWhere { organization :: Array String
                  , labTeamDepts :: Array String

                  , role         :: Maybe String

                  , office       :: Maybe String
                  , country      :: Maybe String
                  , city         :: Maybe String

                  , touch        :: Maybe ContactTouch

                  , entry        :: Maybe String
                  , exit         :: Maybe String
                  }
derive instance Eq ContactWhere
derive instance Generic ContactWhere _
instance Show ContactWhere where
  show = genericShow
instance Argonaut.DecodeJson ContactWhere where
  decodeJson = genericSumDecodeJson
instance Argonaut.EncodeJson ContactWhere where
  encodeJson = genericSumEncodeJson


data ContactTouch =
     ContactTouch { mail      :: Maybe String
                  , phone     :: Maybe String
                  , url       :: Maybe String
                  }
derive instance Eq ContactTouch
derive instance Generic ContactTouch _
instance Show ContactTouch where
  show = genericShow
instance Argonaut.DecodeJson ContactTouch where
  decodeJson = genericSumDecodeJson
instance Argonaut.EncodeJson ContactTouch where
  encodeJson = genericSumEncodeJson

