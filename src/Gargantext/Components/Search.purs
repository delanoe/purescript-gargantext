module Gargantext.Components.Search where

------------------------------------------------------------------------
import Data.Argonaut as Argonaut
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))

import Gargantext.Prelude (class Eq, class Read, class Show)

import Gargantext.Components.Category.Types (Category)
import Gargantext.Utils.Argonaut (genericSumDecodeJson, genericSumEncodeJson, genericEnumDecodeJson, genericEnumEncodeJson)


-- Example:
--   [["machine","learning"],["artificial","intelligence"]]
-- This searches for documents with "machine learning" or "artificial intelligence"
type TextQuery = Array (Array String)
------------------------------------------------------------------------
data SearchType = SearchDoc | SearchContact

derive instance eqSearchType :: Eq SearchType
derive instance genericSearchType :: Generic SearchType _
instance showSearchType :: Show SearchType where
  show = genericShow
instance decodeJsonSearchType :: Argonaut.DecodeJson SearchType where
  decodeJson = genericEnumDecodeJson
instance encodeJsonSearchType :: Argonaut.EncodeJson SearchType where
  encodeJson = genericEnumEncodeJson
------------------------------------------------------------------------

data SearchQuery =
  SearchQuery { query    :: Array String
              , expected :: SearchType
              }

derive instance eqSearchQuery :: Eq SearchQuery
derive instance genericSearchQuery :: Generic SearchQuery _
instance showSearchQuery :: Show SearchQuery where
  show = genericShow
instance decodeJsonSearchQuery :: Argonaut.DecodeJson SearchQuery where
  decodeJson = genericSumDecodeJson
instance encodeJsonSearchQuery :: Argonaut.EncodeJson SearchQuery where
  encodeJson = genericSumEncodeJson

------------------------------------------------------------------------
------------------------------------------------------------------------
data SearchResult = SearchResult { result :: SearchResultTypes }

derive instance eqSearchResult :: Eq SearchResult
derive instance genericSearchResult :: Generic SearchResult _
instance showSearchResult :: Show SearchResult where
  show = genericShow
instance decodeJsonSearchResult :: Argonaut.DecodeJson SearchResult where
  decodeJson = genericSumDecodeJson
instance encodeJsonSearchResult :: Argonaut.EncodeJson SearchResult where
  encodeJson = genericSumEncodeJson


------------------------------------------------------------------------
data SearchResultTypes = SearchResultDoc     { docs     :: Array Document}
                       | SearchNoResult      { message  :: String        }
                       | SearchResultContact { contacts :: Array Contact }

derive instance eqSearchResultTypes :: Eq SearchResultTypes
derive instance genericSearchResultTypes :: Generic SearchResultTypes _
instance showSearchResultTypes :: Show SearchResultTypes where
  show = genericShow
instance decodeJsonSearchResultTypes :: Argonaut.DecodeJson SearchResultTypes where
  decodeJson = genericSumDecodeJson
instance encodeJsonSearchResultTypes :: Argonaut.EncodeJson SearchResultTypes where
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

derive instance eqDocument :: Eq Document
derive instance genericDocument :: Generic Document _
instance showDocument :: Show Document where
  show = genericShow
instance decodeJsonDocument :: Argonaut.DecodeJson Document where
  decodeJson = genericSumDecodeJson
instance encodeJsonDocument :: Argonaut.EncodeJson Document where
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

derive instance eqHyperdataRowDocument :: Eq HyperdataRowDocument
derive instance genericHyperdataRowDocument :: Generic HyperdataRowDocument _
instance showHyperdataRowDocument :: Show HyperdataRowDocument where
  show = genericShow
instance decodeJsonHyperdataRowDocument :: Argonaut.DecodeJson HyperdataRowDocument where
  decodeJson = genericSumDecodeJson
instance encodeJsonHyperdataRowDocument :: Argonaut.EncodeJson HyperdataRowDocument where
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

derive instance eqContact :: Eq Contact
derive instance genericContact :: Generic Contact _
instance showContact :: Show Contact where
  show = genericShow
instance decodeJsonContact :: Argonaut.DecodeJson Contact where
  decodeJson = genericSumDecodeJson
instance encodeJsonContact :: Argonaut.EncodeJson Contact where
  encodeJson = genericSumEncodeJson


data HyperdataRowContact =
     HyperdataRowContact { firstname    :: String
                         , lastname   :: String
                         , labs       :: String
                         }
derive instance eqHyperdataRowContact :: Eq HyperdataRowContact
derive instance genericHyperdataRowContact :: Generic HyperdataRowContact _
instance showHyperdataRowContact :: Show HyperdataRowContact where
  show = genericShow
instance decodeJsonHyperdataRowContact :: Argonaut.DecodeJson HyperdataRowContact where
  decodeJson = genericSumDecodeJson
instance encodeJsonHyperdataRowContact :: Argonaut.EncodeJson HyperdataRowContact where
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
derive instance eqHyperdataContact :: Eq HyperdataContact
derive instance genericHyperdataContact :: Generic HyperdataContact _
instance showHyperdataContact :: Show HyperdataContact where
  show = genericShow
instance decodeJsonHyperdataContact :: Argonaut.DecodeJson HyperdataContact where
  decodeJson = genericSumDecodeJson
instance encodeJsonHyperdataContact :: Argonaut.EncodeJson HyperdataContact where
  encodeJson = genericSumEncodeJson

-------
data ContactWho =
     ContactWho { id          :: Maybe String
                , firstName   :: Maybe String
                , lastName    :: Maybe String
                , keywords :: Array String
                , freetags :: Array String
                }
derive instance eqContactWho :: Eq ContactWho
derive instance genericContactWho :: Generic ContactWho _
instance showContactWho :: Show ContactWho where
  show = genericShow
instance decodeJsonContactWho :: Argonaut.DecodeJson ContactWho where
  decodeJson = genericSumDecodeJson
instance encodeJsonContactWho :: Argonaut.EncodeJson ContactWho where
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
derive instance eqContactWhere :: Eq ContactWhere
derive instance genericContactWhere :: Generic ContactWhere _
instance showContactWhere :: Show ContactWhere where
  show = genericShow
instance decodeJsonContactWhere :: Argonaut.DecodeJson ContactWhere where
  decodeJson = genericSumDecodeJson
instance encodeJsonContactWhere :: Argonaut.EncodeJson ContactWhere where
  encodeJson = genericSumEncodeJson


data ContactTouch =
     ContactTouch { mail      :: Maybe String
                  , phone     :: Maybe String
                  , url       :: Maybe String
                  }
derive instance eqContactTouch :: Eq ContactTouch
derive instance genericContactTouch :: Generic ContactTouch _
instance showContactTouch :: Show ContactTouch where
  show = genericShow
instance decodeJsonContactTouch :: Argonaut.DecodeJson ContactTouch where
  decodeJson = genericSumDecodeJson
instance encodeJsonContactTouch :: Argonaut.EncodeJson ContactTouch where
  encodeJson = genericSumEncodeJson

