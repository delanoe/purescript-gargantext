module Gargantext.Components.Forest.Tree.Node.Action.Search.Types where

import Gargantext.Prelude

import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Gargantext.Components.GraphQL.IMT as GQLIMT
import Gargantext.Components.Lang (Lang)
import Gargantext.Components.ListSelection.Types as ListSelection
import Gargantext.Config.REST (AffRESTError, RESTError)
import Gargantext.Ends (class ToUrl, backendUrl)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session(..), post)
import Gargantext.Types as GT
import Simple.JSON as JSON
import URI.Extra.QueryPairs as QP
import URI.Query as Q

type Search = { databases :: Database
              , datafield :: Maybe DataField
              , url       :: String
              , lang      :: Maybe Lang
              , node_id   :: Maybe Int
              , term      :: String
              , years     :: Array String
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
  show Gargantext   = "Gargantext (Beta)"
  show (External _) = "Databases (APIs)" -- <> show x
  show Web          = "Web"
  show Files        = "Files"
instance Doc DataField where
  doc Gargantext   = "All Gargantext Database"
  doc (External _) = "External (scientific) databases"
  doc Web          = "To launch an analysis on french news (FR only supported for now): put your query, select FR and launch with button on bottom."
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
               , Arxiv
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
              | Arxiv
              | HAL (Maybe Org)
              | IsTex
              | IsTex_Advanced
              | Isidore
--              | News
--              | SocialNetworks
derive instance Generic Database _
instance Show Database where
  show All_Databases  = "All Databases"
  show PubMed         = "PubMed"
  show Arxiv          = "Arxiv"
  show (HAL _)        = "HAL"
  show IsTex          = "IsTex"
  show IsTex_Advanced = "IsTex_Advanced"
  show Isidore        = "Isidore"
  show Empty          = "Empty"
--  show News   = "News"
--  show SocialNetworks = "Social Networks"

instance Doc Database where
  doc All_Databases  = "All databases"
  doc PubMed         = "All Medical publications"
  doc Arxiv          = "Arxiv"
  doc (HAL _)        = "All open science (archives ouvertes)"
  doc IsTex          = "All Elsevier enriched by CNRS/INIST"
  doc IsTex_Advanced = "IsTex advanced search"
  doc Isidore        = "All (French) Social Sciences"
  doc Empty          = "Empty"
--  doc News        = "Web filtered by News"
--  doc SocialNetworks = "Web filtered by MicroBlogs"

instance Read Database where
  read :: String -> Maybe Database
  read "All Databases"  = Just All_Databases
  read "PubMed"         = Just PubMed
  read "Arxiv"          = Just Arxiv
  read "HAL"            = Just $ HAL Nothing
  read "Isidore"        = Just Isidore
  read "IsTex"          = Just IsTex
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
-- NOTE: IMT organizations are fetched via GraphQL from the backend

data IMT_org = All_IMT
             | IMT_org GQLIMT.School

derive instance Ord IMT_org
derive instance Eq IMT_org

instance Show IMT_org where
  show All_IMT                        = "All_IMT"
  show (IMT_org { school_shortName }) = school_shortName

instance Read IMT_org where
  read "All_IMT"             = Just All_IMT
  read _                     = Nothing

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
  , selection :: ListSelection.Selection
  }
derive instance Generic SearchQuery _
derive instance Newtype SearchQuery _
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
  writeImpl (SearchQuery { databases, datafield, lang, node_id, query, selection }) =
    JSON.writeImpl { query: query -- String.replace (String.Pattern "\"") (String.Replacement "\\\"") query
                   , databases
                   , datafield
                   , lang: maybe "EN" show lang
                   , node_id: fromMaybe 0 node_id
                   , flowListWith: selection
                   }

defaultSearchQuery :: SearchQuery
defaultSearchQuery = SearchQuery
  { query     : ""
  , databases : Empty
  , datafield : Nothing
  , files_id  : []
  , lang      : Nothing
  , limit     : Nothing
  , node_id   : Nothing
  , offset    : Nothing
  , order     : Nothing
  , selection : ListSelection.MyListsFirst
  }

performSearch :: Session -> Int -> SearchQuery -> AffRESTError GT.AsyncTaskWithType
performSearch session nodeId q = do
  eTask :: Either RESTError GT.AsyncTask <- post session p q
  pure $ (\task -> GT.AsyncTaskWithType { task, typ: GT.Query }) <$> eTask
  where
    p = GR.NodeAPI GT.Corpus (Just nodeId) $ GT.asyncTaskTypePath GT.Query
