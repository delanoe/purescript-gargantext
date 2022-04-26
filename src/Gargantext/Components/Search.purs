module Gargantext.Components.Search where

import Gargantext.Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Gargantext.Utils.SimpleJSON as GUSJ
import Simple.JSON as JSON
import Simple.JSON.Generics as JSONG

-- Example:
--   [["machine","learning"],["artificial","intelligence"]]
-- This searches for documents with "machine learning" or "artificial intelligence"
type TextQuery = Array (Array String)
------------------------------------------------------------------------
data SearchType = SearchDoc | SearchContact

derive instance Generic SearchType _
instance Eq SearchType where eq = genericEq
instance Show SearchType where show = genericShow
instance JSON.ReadForeign SearchType where readImpl = JSONG.enumSumRep
instance JSON.WriteForeign SearchType where writeImpl = JSON.writeImpl <<< show
------------------------------------------------------------------------

newtype SearchQuery = SearchQuery
  { query     :: Array String
  , expected  :: SearchType
  }

derive instance Generic SearchQuery _
derive instance Newtype SearchQuery _
instance Eq SearchQuery where eq = genericEq
instance Show SearchQuery where show = genericShow
derive newtype instance JSON.ReadForeign SearchQuery
derive newtype instance JSON.WriteForeign SearchQuery

------------------------------------------------------------------------
------------------------------------------------------------------------
newtype SearchResult = SearchResult { result :: SearchResultTypes }
derive instance Generic SearchResult _
derive instance Newtype SearchResult _
instance Eq SearchResult where eq = genericEq
instance Show SearchResult where show = genericShow
derive newtype instance JSON.ReadForeign SearchResult
derive newtype instance JSON.WriteForeign SearchResult

------------------------------------------------------------------------
data SearchResultTypes = SearchResultDoc     { docs     :: Array Document }
                       | SearchNoResult      { message  :: String         }
                       | SearchResultContact { contacts :: Array Contact  }
derive instance Generic SearchResultTypes _
instance Eq SearchResultTypes where eq = genericEq
instance Show SearchResultTypes where show = genericShow
instance JSON.ReadForeign SearchResultTypes where readImpl = GUSJ.taggedSumRep
instance JSON.WriteForeign SearchResultTypes where
  writeImpl (SearchResultDoc s)     = JSON.writeImpl s
  writeImpl (SearchNoResult s)      = JSON.writeImpl s
  writeImpl (SearchResultContact s) = JSON.writeImpl s

------------------------------------------------------------------------
newtype Document =
  Document { id         :: Int
           , created    :: String
           , title      :: String
           , hyperdata  :: HyperdataRowDocument
           , category   :: Int
           , score      :: Int
           }
derive instance Generic Document _
derive instance Newtype Document _
instance Eq Document where eq = genericEq
instance Show Document where show = genericShow
derive newtype instance JSON.ReadForeign Document
derive newtype instance JSON.WriteForeign Document

------------------------------------------------------------------------
newtype HyperdataRowDocument =
  HyperdataRowDocument { abstract           :: Maybe String
                       , authors            :: Maybe String
                       , bdd                :: Maybe String
                       , doi                :: Maybe String
                       , institutes         :: Maybe String
                       , language_iso2      :: Maybe String
                       , page               :: Maybe Int
                       , publication_date   :: Maybe String
                       , publication_day    :: Maybe Int
                       , publication_hour   :: Maybe Int
                       , publication_minute :: Maybe Int
                       , publication_month  :: Maybe Int
                       , publication_second :: Maybe Int
                       , publication_year   :: Maybe Int
                       , source             :: Maybe String
                       , title              :: Maybe String
                       , url                :: Maybe String
                       , uniqId             :: Maybe String
                       , uniqIdBdd          :: Maybe String
                       }

derive instance Generic HyperdataRowDocument _
instance Eq HyperdataRowDocument where eq = genericEq
instance Show HyperdataRowDocument where show = genericShow
derive newtype instance JSON.ReadForeign HyperdataRowDocument
derive newtype instance JSON.WriteForeign HyperdataRowDocument

------------------------------------------------------------------------
------------------------------------------------------------------------
------------------------------------------------------------------------
newtype Contact =
  Contact  { c_id         :: Int
           , c_created    :: String
           , c_hyperdata  :: HyperdataRowContact
           , c_score      :: Int
           , c_annuaireId :: Int
           }

derive instance Generic Contact _
instance Eq Contact where eq = genericEq
instance Show Contact where show = genericShow
derive newtype instance JSON.ReadForeign Contact
derive newtype instance JSON.WriteForeign Contact

newtype HyperdataRowContact =
  HyperdataRowContact { firstname :: String
                      , lastname  :: String
                      , labs      :: String
                      }
derive instance Generic HyperdataRowContact _
instance Eq HyperdataRowContact where eq = genericEq
instance Show HyperdataRowContact where show = genericShow
derive newtype instance JSON.ReadForeign HyperdataRowContact
derive newtype instance JSON.WriteForeign HyperdataRowContact


newtype HyperdataContact =
   HyperdataContact { bdd           :: Maybe String
                   , who            :: Maybe ContactWho
                   , "where"        :: Array ContactWhere
                   , title          :: Maybe String
                   , source         :: Maybe String
                   , lastValidation :: Maybe String
                   , uniqIdBdd      :: Maybe String
                   , uniqId         :: Maybe String
                   }
derive instance Generic HyperdataContact _
instance Eq HyperdataContact where eq = genericEq
instance Show HyperdataContact where show = genericShow
derive newtype instance JSON.ReadForeign HyperdataContact
derive newtype instance JSON.WriteForeign HyperdataContact

-------
newtype ContactWho =
  ContactWho { id        :: Maybe String
             , firstName :: Maybe String
             , lastName  :: Maybe String
             , keywords  :: Array String
             , freetags  :: Array String
             }
derive instance Generic ContactWho _
instance Eq ContactWho where eq = genericEq
instance Show ContactWho where show = genericShow
derive newtype instance JSON.ReadForeign ContactWho
derive newtype instance JSON.WriteForeign ContactWho


newtype ContactWhere =
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
derive instance Generic ContactWhere _
instance Eq ContactWhere where eq = genericEq
instance Show ContactWhere where show = genericShow
derive newtype instance JSON.ReadForeign ContactWhere
derive newtype instance JSON.WriteForeign ContactWhere


newtype ContactTouch =
 ContactTouch { mail   :: Maybe String
               , phone :: Maybe String
               , url   :: Maybe String
               }
derive instance Generic ContactTouch _
instance Eq ContactTouch where eq = genericEq
instance Show ContactTouch where show = genericShow
derive newtype instance JSON.ReadForeign ContactTouch
derive newtype instance JSON.WriteForeign ContactTouch
