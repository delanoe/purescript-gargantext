module Gargantext.Components.Search.Types where

import Data.Array (concat)
import Data.Argonaut (class EncodeJson, encodeJson, jsonEmptyObject, (:=), (~>))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import URI.Extra.QueryPairs as QP
import URI.Query as Q

import Gargantext.Prelude (class Eq, class Ord, class Show, bind, map, pure, show, ($), (<>))

import Gargantext.Ends (class ToUrl, backendUrl)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session(..), post)
import Gargantext.Types as GT
import Gargantext.Utils (id)

------------------------------------------------------------------------
class Doc a where
  doc :: a -> String

------------------------------------------------------------------------
-- | Lang search specifications
allLangs :: Array Lang
allLangs = [ EN
           , FR
           , Universal
           , No_extraction
           ]

data Lang = FR | EN | Universal | No_extraction

instance showLang :: Show Lang where
  show FR = "FR"
  show EN = "EN"
  show Universal = "All"
  show No_extraction = "Nothing"

derive instance eqLang :: Eq Lang

readLang :: String -> Maybe Lang
readLang "FR"  = Just FR
readLang "EN"  = Just EN
readLang "All" = Just Universal
readLang "Nothing" = Just No_extraction
readLang _           = Nothing

instance encodeJsonLang :: EncodeJson Lang where
  encodeJson a = encodeJson (show a)

------------------------------------------------------------------------
-- | DataField search specifications

dataFields :: Array DataField
dataFields = [ Gargantext
             , Web
             , External Nothing
             -- , Files
             ]

data DataField = Gargantext
               | External (Maybe Database)
               | Web
               | Files

instance showDataField :: Show DataField where
  show Gargantext   = "Gargantext"
  show (External x) = "External" -- <> show x
  show Web          = "Web"
  show Files        = "Files"

instance docDataField :: Doc DataField where
  doc Gargantext   = "All Gargantext Database"
  doc (External _) = "External (scientific) databases"
  doc Web          = "All the web crawled with meta-search-engine SearX"
  doc Files        = "Zip files with formats.."


derive instance eqDataField :: Eq DataField

{-
instance eqDataField :: Eq DataField where
  eq Gargantext Gargantext = true
  eq (External _) (External _) = true
  eq Web Web = true
  eq _ _ = false
  -}
------------------------------------------------------------------------
-- | Database search specifications

allDatabases :: Array Database
allDatabases = [ PubMed
               , HAL Nothing
               , IsTex
               , Isidore
               --, Web
               --, News
               --, SocialNetworks
               ]

data Database = All_Databases
              | PubMed
              | HAL (Maybe Org)
              | IsTex
              | Isidore
--              | News
--              | SocialNetworks

instance showDatabase :: Show Database where
  show All_Databases= "All Databases"
  show PubMed = "PubMed"
  show (HAL _)= "HAL"
  show IsTex  = "IsTex"
  show Isidore= "Isidore"
--  show News   = "News"
--  show SocialNetworks = "Social Networks"

instance docDatabase :: Doc Database where
  doc All_Databases = "All databases"
  doc PubMed      = "All Medical publications"
  doc (HAL _)     = "All open science (archives ouvertes)"
  doc IsTex       = "All Elsevier enriched by CNRS/INIST"
  doc Isidore     = "All (French) Social Sciences"
--  doc News        = "Web filtered by News"
--  doc SocialNetworks = "Web filtered by MicroBlogs"

readDatabase :: String -> Maybe Database
readDatabase "All Databases" = Just All_Databases
readDatabase "PubMed" = Just PubMed
readDatabase "HAL"    = Just $ HAL Nothing
readDatabase "IsTex"  = Just IsTex
readDatabase "Isidore"= Just Isidore
-- readDatabase "Web"    = Just Web
-- readDatabase "News"   = Just News
-- readDatabase "Social Networks" = Just SocialNetworks
readDatabase _        = Nothing

derive instance eqDatabase :: Eq Database

instance encodeJsonDatabase :: EncodeJson Database where
  encodeJson a = encodeJson (show a)
------------------------------------------------------------------------
-- | Organization specifications

allOrgs :: Array Org
allOrgs = [ All_Orgs
          , IMT  $ Set.fromFoldable []
          , CNRS $ Set.fromFoldable []
          ]

data Org = All_Orgs
         | CNRS   (Set StructId)
         | Others (Set StructId)
         | IMT    (Set IMT_org)

type StructId = Int

instance showOrg :: Show Org where
  show All_Orgs   = "All_Orgs"
  show (CNRS _)   = "CNRS"
  show (IMT  _)   = "IMT"
  show (Others _) = "Others"

readOrg :: String -> Maybe Org
readOrg "All_Orgs" = Just $ All_Orgs
readOrg "CNRS"     = Just $ CNRS   $ Set.fromFoldable []
readOrg "IMT"      = Just $ IMT    $ Set.fromFoldable []
readOrg "Others"   = Just $ Others $ Set.fromFoldable []
readOrg _          = Nothing

derive instance eqOrg :: Eq Org

instance encodeJsonOrg :: EncodeJson Org where
  encodeJson a = encodeJson (show a)

------------------------------------------------------------------------

allIMTorgs :: Array IMT_org
allIMTorgs = [All_IMT] <> allIMTSubOrgs

allIMTSubOrgs :: Array IMT_org
allIMTSubOrgs = [ ARMINES
                , Eurecom
                , IMT_Atlantique
                , IMT_Business_School
                , IMT_Lille_Douai
                , IMT_Mines_ALES
                , IMT_Mines_Albi
                , Institut_MinesTelecom_Paris
                , MINES_ParisTech
                , Mines_Douai
                , Mines_Nantes
                , Mines_SaintEtienne
                , Telecom_Bretagne
                , Telecom_Ecole_de_Management
                , Telecom_Lille
                , Telecom_ParisTech
                , Telecom_SudParis
                ]

data IMT_org = All_IMT
             | ARMINES
             | Eurecom
             | IMT_Atlantique
             | IMT_Business_School
             | IMT_Lille_Douai
             | IMT_Mines_ALES
             | IMT_Mines_Albi
             | Institut_MinesTelecom_Paris
             | MINES_ParisTech
             | Mines_Douai
             | Mines_Nantes
             | Mines_SaintEtienne
             | Telecom_Bretagne
             | Telecom_Ecole_de_Management
             | Telecom_Lille
             | Telecom_ParisTech
             | Telecom_SudParis

derive instance ordIMT_org :: Ord IMT_org
derive instance eqIMT_org  :: Eq IMT_org

instance showIMT_org :: Show IMT_org where
  show All_IMT             = "All_IMT"
  show ARMINES             = "ARMINES"
  show Eurecom             = "Eurecom"
  show IMT_Atlantique      = "IMT_Atlantique"
  show IMT_Business_School = "IMT_Business_School"
  show IMT_Lille_Douai     = "IMT_Lille_Douai"
  show IMT_Mines_ALES      = "IMT_Mines_ALES"
  show IMT_Mines_Albi      = "IMT_Mines_Albi"
  show Institut_MinesTelecom_Paris = "Institut_MinesTelecom_Paris"
  show MINES_ParisTech     = "MINES_ParisTech"
  show Mines_Douai         = "Mines_Douai"
  show Mines_Nantes        = "Mines_Nantes"
  show Mines_SaintEtienne  = "Mines_SaintEtienne"
  show Telecom_Bretagne    = "Telecom_Bretagne"
  show Telecom_Ecole_de_Management = "Telecom_Ecole_de_Management"
  show Telecom_Lille       = "Telecom_Lille"
  show Telecom_ParisTech   = "Telecom_ParisTech"
  show Telecom_SudParis    = "Telecom_SudParis"

readIMT_org :: String -> Maybe IMT_org
readIMT_org "All_IMT"             = Just All_IMT
readIMT_org "ARMINES"             = Just ARMINES
readIMT_org "Eurecom"             = Just Eurecom
readIMT_org "IMT_Atlantique"      = Just IMT_Atlantique
readIMT_org "IMT_Business_School" = Just IMT_Business_School
readIMT_org "IMT_Lille_Douai"     = Just IMT_Lille_Douai
readIMT_org "IMT_Mines_ALES"      = Just IMT_Mines_ALES
readIMT_org "IMT_Mines_Albi"      = Just IMT_Mines_Albi
readIMT_org "Institut_MinesTelecom_Paris" = Just Institut_MinesTelecom_Paris
readIMT_org "MINES_ParisTech"     = Just MINES_ParisTech
readIMT_org "Mines_Douai"         = Just Mines_Douai
readIMT_org "Mines_Nantes"        = Just Mines_Nantes
readIMT_org "Mines_SaintEtienne"  = Just Mines_SaintEtienne
readIMT_org "Telecom_Bretagne"    = Just Telecom_Bretagne
readIMT_org "Telecom_Ecole_de_Management" = Just Telecom_Ecole_de_Management
readIMT_org "Telecom_Lille"       = Just Telecom_Lille
readIMT_org "Telecom_ParisTech"   = Just Telecom_ParisTech
readIMT_org "Telecom_SudParis"    = Just Telecom_SudParis
readIMT_org _                     = Nothing

imtStructId :: IMT_org -> Array StructId
imtStructId All_IMT           = concat $ map imtStructId allIMTSubOrgs
imtStructId Mines_Douai       = [224096]
imtStructId Telecom_Lille     = [144103]
imtStructId Mines_Nantes      = [84538]
imtStructId ARMINES           = [300104]
imtStructId Telecom_ParisTech = [300362]
imtStructId Telecom_Bretagne  = [301262]
imtStructId Telecom_Ecole_de_Management = [301442]
imtStructId MINES_ParisTech   = [301492]
imtStructId Institut_MinesTelecom_Paris = [302102]
imtStructId Eurecom           = [421532]
imtStructId IMT_Lille_Douai   = [497330]
imtStructId Telecom_SudParis  = [352124]
imtStructId IMT_Atlantique    = [481355]
imtStructId IMT_Mines_Albi    = [469216]
imtStructId IMT_Business_School = [542824]
imtStructId IMT_Mines_ALES     = [6279]
imtStructId Mines_SaintEtienne = [29212]

------------------------------------------------------------------------
data SearchOrder
  = DateAsc
  | DateDesc
  | TitleAsc
  | TitleDesc
  | ScoreAsc
  | ScoreDesc

instance showSearchOrder :: Show SearchOrder where
  show DateAsc = "DateAsc"
  show DateDesc = "DateDesc"
  show TitleAsc = "TitleAsc"
  show TitleDesc = "TitleDesc"
  show ScoreAsc = "ScoreAsc"
  show ScoreDesc = "ScoreDesc"

------------------------------------------------------------------------

newtype SearchQuery = SearchQuery
  { query     :: String
  , databases :: Array Database
  , datafield :: Maybe DataField
  , files_id  :: Array String
  , lang      :: Maybe Lang
  , limit     :: Maybe Int
  , node_id   :: Maybe Int
  , offset    :: Maybe Int
  , order     :: Maybe SearchOrder
  }

derive instance newtypeSearchQuery :: Newtype SearchQuery _

defaultSearchQuery :: SearchQuery
defaultSearchQuery = SearchQuery
  { query: ""
  , databases: []
  , datafield: Nothing
  , files_id : []
  , lang    : Nothing
  , limit: Nothing
  , node_id : Nothing
  , offset: Nothing
  , order: Nothing
  }

instance toUrlSessionSearchQuery :: ToUrl Session SearchQuery where
  toUrl (Session {backend}) q = backendUrl backend q2
    where q2 = "new" <> Q.print (GT.toQuery q)
  
instance searchQueryToQuery :: GT.ToQuery SearchQuery where
  toQuery (SearchQuery {offset, limit, order}) =
    QP.print id id $ QP.QueryPairs
                   $ pair "offset" offset
                   <> pair "limit" limit
                   <> pair "order" order
    where pair :: forall a. Show a => String -> Maybe a -> Array (Tuple QP.Key (Maybe QP.Value))
          pair k = maybe [] $ \v ->
            [ QP.keyFromString k /\ Just (QP.valueFromString $ show v) ]

instance encodeJsonSearchQuery :: EncodeJson SearchQuery where
  encodeJson (SearchQuery {query, databases, datafield, node_id, lang})
    =  "query"      := query
    -- ~> "datafield"  := "" -- fromMaybe "" datafield
    ~> "databases"  := databases
    ~> "lang"       := maybe "EN" show lang
    ~> "node_id"    := fromMaybe 0 node_id
    -- ~> "files_id"   := files_id
    ~> jsonEmptyObject

performSearch :: Session -> Int -> SearchQuery -> Aff GT.AsyncTaskWithType
performSearch session nodeId q = do
  task <- post session p q
  pure $ GT.AsyncTaskWithType {task, typ: GT.Query}
  where
    p = GR.NodeAPI GT.Corpus (Just nodeId) $ GT.asyncTaskTypePath GT.Query
