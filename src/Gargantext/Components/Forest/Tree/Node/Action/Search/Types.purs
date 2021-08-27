module Gargantext.Components.Forest.Tree.Node.Action.Search.Types where

import Data.Array (concat)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Simple.JSON as JSON
import URI.Extra.QueryPairs as QP
import URI.Query as Q


import Gargantext.Prelude

import Gargantext.Components.Lang (Lang)
import Gargantext.Config.REST (RESTError)
import Gargantext.Ends (class ToUrl, backendUrl)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session(..), post)
import Gargantext.Types as GT

type Search = { databases :: Database
              , datafield :: Maybe DataField
              , url       :: String
              , lang      :: Maybe Lang
              , node_id   :: Maybe Int
              , term      :: String
              }

isIsTex_Advanced :: Maybe DataField -> Boolean
isIsTex_Advanced ( Just
          ( External
            ( Just ( IsTex_Advanced)
            )
          )
        ) = true
isIsTex_Advanced _ = false


------------------------------------------------------------------------
class Doc a where
  doc :: a -> String

------------------------------------------------------------------------
-- | DataField search specifications

dataFields :: Array DataField
dataFields = [ Gargantext
             , External Nothing
             , Web
             -- , Files
             ]

data DataField = Gargantext
               | External (Maybe Database)
               | Web
               | Files

derive instance Generic DataField _
instance Show DataField where
  show Gargantext   = "Gargantext"
  show (External _) = "Databases (APIs)" -- <> show x
  show Web          = "Soon: web"
  show Files        = "Files"
instance Doc DataField where
  doc Gargantext   = "All Gargantext Database"
  doc (External _) = "External (scientific) databases"
  doc Web          = "All the web crawled with meta-search-engine SearX"
  doc Files        = "Zip files with formats.."
derive instance Eq DataField
instance JSON.WriteForeign DataField where
  writeImpl (External (Just db)) = JSON.writeImpl $ "External " <> show db
  writeImpl Web = JSON.writeImpl $ "Web"
  writeImpl f = JSON.writeImpl $ show f

----------------------------------------
data DataOriginApi = InternalOrigin { api :: Database }
                   | ExternalOrigin { api :: Database }
derive instance Generic DataOriginApi _
instance Show DataOriginApi where
  show (InternalOrigin io) = "InternalOrigin " <> show io.api
  show (ExternalOrigin io) = "ExternalOrigin " <> show io.api
derive instance Eq DataOriginApi
instance JSON.WriteForeign DataOriginApi where
  writeImpl (InternalOrigin { api }) = JSON.writeImpl { api }
  writeImpl (ExternalOrigin { api }) = JSON.writeImpl { api }

datafield2dataOriginApi :: DataField -> DataOriginApi
datafield2dataOriginApi (External (Just a)) = ExternalOrigin { api : a }
datafield2dataOriginApi _                   = InternalOrigin { api : IsTex } -- TODO fixme 

------------------------------------------------------------------------
-- | Database search specifications

datafield2database :: DataField -> Database
datafield2database (External (Just x)) = x
datafield2database _                   = Empty

allDatabases :: Array Database
allDatabases = [ Empty
               , PubMed
               , HAL Nothing
               , IsTex
               , IsTex_Advanced
               , Isidore
               --, Web
               --, News
               --, SocialNetworks
               ]

data Database = All_Databases
              | Empty
              | PubMed
              | HAL (Maybe Org)
              | IsTex
              | IsTex_Advanced
              | Isidore
--              | News
--              | SocialNetworks
derive instance Generic Database _
instance Show Database where
  show All_Databases= "All Databases"
  show PubMed = "PubMed"
  show (HAL _)= "HAL"
  show IsTex  = "IsTex"
  show IsTex_Advanced  = "IsTex_Advanced"
  show Isidore= "Isidore"
  show Empty  = "Empty"
--  show News   = "News"
--  show SocialNetworks = "Social Networks"

instance Doc Database where
  doc All_Databases = "All databases"
  doc PubMed      = "All Medical publications"
  doc (HAL _)     = "All open science (archives ouvertes)"
  doc IsTex       = "All Elsevier enriched by CNRS/INIST"
  doc IsTex_Advanced = "IsTex advanced search"
  doc Isidore     = "All (French) Social Sciences"
  doc Empty       = "Empty"
--  doc News        = "Web filtered by News"
--  doc SocialNetworks = "Web filtered by MicroBlogs"

instance Read Database where
  read :: String -> Maybe Database
  read "All Databases" = Just All_Databases
  read "PubMed" = Just PubMed
  read "HAL"    = Just $ HAL Nothing
  read "Isidore"= Just Isidore
  read "IsTex"  = Just IsTex
  read "IsTex_Advanced" = Just IsTex_Advanced
  -- read "Web"    = Just Web
  -- read "News"   = Just News
  -- read "Social Networks" = Just SocialNetworks
  read _        = Nothing

derive instance Eq Database
instance JSON.WriteForeign Database where writeImpl = JSON.writeImpl <<< show

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
derive instance Generic Org _
instance Show Org where
  show All_Orgs   = "All_Orgs"
  show (CNRS _)   = "CNRS"
  show (IMT  _)   = "IMT"
  show (Others _) = "Others"

instance Read Org where
  read "All_Orgs" = Just $ All_Orgs
  read "CNRS"     = Just $ CNRS   $ Set.fromFoldable []
  read "IMT"      = Just $ IMT    $ Set.fromFoldable []
  read "Others"   = Just $ Others $ Set.fromFoldable []
  read _          = Nothing
derive instance Eq Org
instance JSON.WriteForeign Org where writeImpl = JSON.writeImpl <<< show

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

derive instance Ord IMT_org
derive instance Eq IMT_org

instance Show IMT_org where
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

instance Read IMT_org where
  read "All_IMT"             = Just All_IMT
  read "ARMINES"             = Just ARMINES
  read "Eurecom"             = Just Eurecom
  read "IMT_Atlantique"      = Just IMT_Atlantique
  read "IMT_Business_School" = Just IMT_Business_School
  read "IMT_Lille_Douai"     = Just IMT_Lille_Douai
  read "IMT_Mines_ALES"      = Just IMT_Mines_ALES
  read "IMT_Mines_Albi"      = Just IMT_Mines_Albi
  read "Institut_MinesTelecom_Paris" = Just Institut_MinesTelecom_Paris
  read "MINES_ParisTech"     = Just MINES_ParisTech
  read "Mines_Douai"         = Just Mines_Douai
  read "Mines_Nantes"        = Just Mines_Nantes
  read "Mines_SaintEtienne"  = Just Mines_SaintEtienne
  read "Telecom_Bretagne"    = Just Telecom_Bretagne
  read "Telecom_Ecole_de_Management" = Just Telecom_Ecole_de_Management
  read "Telecom_Lille"       = Just Telecom_Lille
  read "Telecom_ParisTech"   = Just Telecom_ParisTech
  read "Telecom_SudParis"    = Just Telecom_SudParis
  read _                     = Nothing

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

instance Show SearchOrder where
  show DateAsc = "DateAsc"
  show DateDesc = "DateDesc"
  show TitleAsc = "TitleAsc"
  show TitleDesc = "TitleDesc"
  show ScoreAsc = "ScoreAsc"
  show ScoreDesc = "ScoreDesc"

------------------------------------------------------------------------

newtype SearchQuery = SearchQuery
  { query     :: String
  , databases :: Database
  , datafield :: Maybe DataField
  , files_id  :: Array String
  , lang      :: Maybe Lang
  , limit     :: Maybe Int
  , node_id   :: Maybe Int
  , offset    :: Maybe Int
  , order     :: Maybe SearchOrder
  }
derive instance Generic SearchQuery _
derive instance Newtype SearchQuery _

defaultSearchQuery :: SearchQuery
defaultSearchQuery = SearchQuery
  { query: ""
  , databases: Empty
  , datafield: Nothing
  , files_id : []
  , lang     : Nothing
  , limit    : Nothing
  , node_id  : Nothing
  , offset   : Nothing
  , order    : Nothing
  }

instance ToUrl Session SearchQuery where
  toUrl (Session {backend}) q = backendUrl backend q2
    where q2 = "new" <> Q.print (GT.toQuery q)
  
instance GT.ToQuery SearchQuery where
  toQuery (SearchQuery {offset, limit, order}) =
    QP.print id id $ QP.QueryPairs
                   $ pair "offset" offset
                   <> pair "limit" limit
                   <> pair "order" order
    where pair :: forall a. Show a => String -> Maybe a -> Array (Tuple QP.Key (Maybe QP.Value))
          pair k = maybe [] $ \v ->
            [ QP.keyFromString k /\ Just (QP.valueFromString $ show v) ]
instance JSON.WriteForeign SearchQuery where
  writeImpl (SearchQuery { databases, datafield, lang, node_id, query }) =
    JSON.writeImpl { query: String.replace (String.Pattern "\"") (String.Replacement "\\\"") query
                   , databases
                   , datafield
                   , lang: maybe "EN" show lang
                   , node_id: fromMaybe 0 node_id
                   }

performSearch :: Session -> Int -> SearchQuery -> Aff (Either RESTError GT.AsyncTaskWithType)
performSearch session nodeId q = do
  eTask :: Either RESTError GT.AsyncTask <- post session p q
  pure $ (\task -> GT.AsyncTaskWithType { task, typ: GT.Query }) <$> eTask
  where
    p = GR.NodeAPI GT.Corpus (Just nodeId) $ GT.asyncTaskTypePath GT.Query
