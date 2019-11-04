module Gargantext.Components.Search.Types where

import Prelude (class Eq, class Show, show, ($), (<>))
import Data.Argonaut (class EncodeJson, class DecodeJson, jsonEmptyObject, (:=), (~>), encodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Gargantext.Ends (class ToUrl, backendUrl)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session(..), post, put)
import Gargantext.Types (class ToQuery, toQuery, NodeType(..))
import Gargantext.Utils (id)
import URI.Extra.QueryPairs as QP
import URI.Query as Q

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
  show Universal = "Universal"
  show No_extraction = "No_extraction"

derive instance eqLang :: Eq Lang

readLang :: String -> Maybe Lang
readLang "FR"  = Just FR
readLang "EN"  = Just EN
readLang "Universal" = Just Universal
readLang "No_extraction" = Just No_extraction
readLang _           = Nothing

instance encodeJsonLang :: EncodeJson Lang where
  encodeJson a = encodeJson (show a)

------------------------------------------------------------------------
-- | Database search specifications
allDatabases :: Array Database
allDatabases = [All, PubMed
               , HAL
               , IsTex
               , Isidore
               ]

data Database = All          | PubMed
              | HAL
              | IsTex
              | Isidore


instance showDatabase :: Show Database where
  show All    = "In Gargantext"
  show PubMed = "PubMed"
  show HAL    = "HAL"
  show IsTex  = "IsTex"
  show Isidore= "Isidore"

readDatabase :: String -> Maybe Database
readDatabase "All"    = Just All
readDatabase "PubMed" = Just PubMed
readDatabase "HAL"    = Just HAL
readDatabase "IsTex"  = Just IsTex
readDatabase "Isidore"= Just Isidore
readDatabase _        = Nothing

derive instance eqDatabase :: Eq Database

instance encodeJsonDatabase :: EncodeJson Database where
  encodeJson a = encodeJson (show a)
------------------------------------------------------------------------
-- | Database Filter specifications
-- filter by organization

allOrgs :: Array Org
allOrgs = [ All_Orgs
          , IMT  {orgs:[]}
          , CNRS {orgs:[]}
          ]

data Org = All_Orgs
         | CNRS   { orgs :: Array Int }
         | IMT    { orgs :: Array IMT_org }
         | Others { orgs :: Array Int }

instance showOrg :: Show Org where
  show All_Orgs = "All__Orgs"
  show (CNRS   _)    = "CNRS"
  show (IMT    _)    = "IMT"
  show (Others _)    = "Others"

readOrg :: String -> Maybe Org
readOrg "All_Orgs" = Just $ All_Orgs
readOrg "CNRS"     = Just $ CNRS {orgs: []}
readOrg "IMT"      = Just $ IMT  {orgs: []}
readOrg "Others"   = Just $ Others  {orgs: []}
readOrg _          = Nothing

instance eqOrg :: Eq Org
  where
    eq All_Orgs All_Orgs = true
    eq (CNRS _) (CNRS _) = true
    eq (IMT  _) (IMT  _) = true
    eq (Others _) (Others _)  = true
    eq _  _        = false

instance encodeJsonOrg :: EncodeJson Org where
  encodeJson a = encodeJson (show a)

------------------------------------------------------------------------
allIMTorgs :: Array IMT_org
allIMTorgs = [ ARMINES
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

data IMT_org = ARMINES
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

instance showIMT_org :: Show IMT_org where
  show ARMINES = "ARMINES"
  show Eurecom = "Eurecom"
  show IMT_Atlantique = "IMT_Atlantique"
  show IMT_Business_School = "IMT_Business_School"
  show IMT_Lille_Douai = "IMT_Lille_Douai"
  show IMT_Mines_ALES = "IMT_Mines_ALES"
  show IMT_Mines_Albi = "IMT_Mines_Albi"
  show Institut_MinesTelecom_Paris = "Institut_MinesTelecom_Paris"
  show MINES_ParisTech = "MINES_ParisTech"
  show Mines_Douai = "Mines_Douai"
  show Mines_Nantes = "Mines_Nantes"
  show Mines_SaintEtienne = "Mines_SaintEtienne"
  show Telecom_Bretagne = "Telecom_Bretagne"
  show Telecom_Ecole_de_Management = "Telecom_Ecole_de_Management"
  show Telecom_Lille = "Telecom_Lille"
  show Telecom_ParisTech = "Telecom_ParisTech"
  show Telecom_SudParis = "Telecom_SudParis"



{-
Mines_Douai 224096
Telecom_Lille 144103
Mines_Nantes 84538
ARMINES 300104
Telecom_ParisTech 300362
Telecom_Bretagne 301262
Telecom_Ecole_de_Management 301442
MINES_ParisTech 301492
Institut_MinesTelecom_Paris 302102
Eurecom 421532
IMT_Lille_Douai 497330
Telecom_SudParis 352124
IMT_Atlantique 481355
IMT_Mines_Albi 469216
IMT_Business_School 542824
IMT_Mines_ALES 6279
Mines_SaintEtienne 29212
-}

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
  , lang     :: Maybe Lang
  , node_id   :: Maybe Int
  , files_id  :: Array String
  , offset    :: Maybe Int
  , limit     :: Maybe Int
  , order     :: Maybe SearchOrder 
  }

derive instance newtypeSearchQuery :: Newtype SearchQuery _

defaultSearchQuery :: SearchQuery
defaultSearchQuery = SearchQuery
  { query: ""
  , databases: allDatabases
  , lang    : Nothing
  , node_id : Nothing
  , files_id : []
  , offset: Nothing
  , limit: Nothing
  , order: Nothing }

instance toUrlSessionSearchQuery :: ToUrl Session SearchQuery where
  toUrl (Session {backend}) q = backendUrl backend q2
    where q2 = "new" <> Q.print (toQuery q)
  
instance searchQueryToQuery :: ToQuery SearchQuery where
  toQuery (SearchQuery {offset, limit, order}) =
    QP.print id id $ QP.QueryPairs $
         pair "offset" offset <> pair "limit" limit <> pair "order" order
    where pair :: forall a. Show a => String -> Maybe a -> Array (Tuple QP.Key (Maybe QP.Value))
          pair k = maybe [] $ \v ->
            [ QP.keyFromString k /\ Just (QP.valueFromString $ show v) ]

instance encodeJsonSearchQuery :: EncodeJson SearchQuery where
  encodeJson (SearchQuery {query, databases, node_id, files_id})
    =   "query"      := query
    ~> "databases"   := databases
    ~>  "node_id"  := fromMaybe 0 node_id
    ~>  "files_id"   := files_id
    ~> jsonEmptyObject


data Category = Trash | Normal | Favorite
derive instance genericFavorite :: Generic Category _
instance showCategory :: Show Category where
  show = genericShow
instance eqCategory :: Eq Category where
  eq = genericEq
instance encodeJsonCategory :: EncodeJson Category where
  encodeJson Trash = encodeJson 0
  encodeJson Normal = encodeJson 1
  encodeJson Favorite = encodeJson 2

favCategory :: Category -> Category
favCategory Normal = Favorite
favCategory Trash = Favorite
favCategory Favorite = Normal

trashCategory :: Category -> Category
trashCategory Normal = Trash
trashCategory Trash = Normal
trashCategory Favorite = Trash


decodeCategory :: Int -> Category
decodeCategory 0 = Trash
decodeCategory 1 = Normal
decodeCategory 2 = Favorite
decodeCategory _ = Normal

newtype CategoryQuery = CategoryQuery {
    nodeIds :: Array Int
  , category :: Category
  }

instance encodeJsonCategoryQuery :: EncodeJson CategoryQuery where
  encodeJson (CategoryQuery post) =
    "ntc_nodesId" := post.nodeIds
    ~> "ntc_category" := encodeJson post.category
    ~> jsonEmptyObject

categoryRoute :: Int -> SessionRoute
categoryRoute nodeId = NodeAPI Node (Just nodeId) "category"

putCategories :: Session -> Int -> CategoryQuery -> Aff (Array Int)
putCategories session nodeId = put session $ categoryRoute nodeId

performSearch :: forall a. DecodeJson a => Session -> SearchQuery -> Aff a
performSearch session q = post session q q

