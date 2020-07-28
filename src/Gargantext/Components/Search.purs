module Gargantext.Components.Search where

------------------------------------------------------------------------
import Data.Argonaut as Argonaut
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Gargantext.Utils.Argonaut (genericSumDecodeJson, genericSumEncodeJson, genericEnumDecodeJson, genericEnumEncodeJson)
import Gargantext.Components.Category (Category)
import Data.Maybe (Maybe(..))
import Gargantext.Prelude (class Eq, class Read, class Show)


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
  decodeJson = genericSumDecodeJson
instance encodeJsonSearchType :: Argonaut.EncodeJson SearchType where
  encodeJson = genericSumEncodeJson
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
data SearchResult = SearchResultDoc     { docs     :: Array Document}
                  | SearchResultContact { contacts :: Array Contact }
                  -- | SearchNoResult      { message  :: String }

derive instance eqSearchResult :: Eq SearchResult
derive instance genericSearchResult :: Generic SearchResult _
instance showSearchResult :: Show SearchResult where
  show = genericShow
instance decodeJsonSearchResult :: Argonaut.DecodeJson SearchResult where
  decodeJson = genericSumDecodeJson
instance encodeJsonSearchResult :: Argonaut.EncodeJson SearchResult where
  encodeJson = genericSumEncodeJson

------------------------------------------------------------------------

data Document =
  Document { id         :: Int
           , created    :: String
           , title      :: String
           , hyperdata  :: HyperdataDocument
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
data HyperdataDocument =
  HyperdataDocument { bdd                :: Maybe String
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

derive instance eqHyperdataDocument :: Eq HyperdataDocument
derive instance genericHyperdataDocument :: Generic HyperdataDocument _
instance showHyperdataDocument :: Show HyperdataDocument where
  show = genericShow
instance decodeJsonHyperdataDocument :: Argonaut.DecodeJson HyperdataDocument where
  decodeJson = genericSumDecodeJson
instance encodeJsonHyperdataDocument :: Argonaut.EncodeJson HyperdataDocument where
  encodeJson = genericSumEncodeJson

------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
data Contact =
  Contact  { id         :: Int
           , created    :: String
           , hyperdata  :: HyperdataContact
           , score      :: Int
           }

derive instance eqContact :: Eq Contact
derive instance genericContact :: Generic Contact _
instance showContact :: Show Contact where
  show = genericShow
instance decodeJsonContact :: Argonaut.DecodeJson Contact where
  decodeJson = genericSumDecodeJson
instance encodeJsonContact :: Argonaut.EncodeJson Contact where
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


