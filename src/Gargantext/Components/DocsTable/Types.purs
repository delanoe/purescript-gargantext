module Gargantext.Components.DocsTable.Types where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Gargantext.Prelude

import Gargantext.Components.Category.Types (Category(..), decodeCategory, Star(..), decodeStar)

data Action
  = MarkCategory Int Category

newtype DocumentsView
  = DocumentsView
    { _id        :: Int
    , category   :: Star
    , date       :: Int
    , ngramCount :: Maybe Int
    , score      :: Maybe Int
    , source     :: String
    , title      :: String
    , url        :: String
    }
derive instance genericDocumentsView :: Generic DocumentsView _
instance eqDocumentsView :: Eq DocumentsView where
  eq = genericEq

{-
derive instance genericDocumentsView :: Generic DocumentsView _
instance showDocumentsView :: Show DocumentsView where
  show = genericShow
instance decodeJsonSearchType :: Argonaut.DecodeJson SearchType where
  decodeJson = genericSumDecodeJson
instance encodeJsonSearchType :: Argonaut.EncodeJson SearchType where
  encodeJson = genericSumEncodeJson
  -}

instance decodeDocumentsView :: DecodeJson DocumentsView where
  decodeJson json = do
    obj <- decodeJson json
    _id <- obj        .: "id"
    category <- obj   .: "category"
    date <- obj       .: "date"
    ngramCount <- obj .: "ngramCount"
    score <- obj      .: "score"
    source <- obj     .: "source"
    title <- obj      .: "title"
    url <- obj        .: "url"
    pure $ DocumentsView { _id, category, date, ngramCount, score, source, title, url }
instance encodeDocumentsView :: EncodeJson DocumentsView where
  encodeJson (DocumentsView dv) =
       "id"        := dv._id
    ~> "category"   := dv.category
    ~> "date"       := dv.date
    ~> "ngramCount" := dv.ngramCount
    ~> "score"      := dv.score
    ~> "source"     := dv.source
    ~> "title"      := dv.title
    ~> "url"        := dv.url
    ~> jsonEmptyObject


newtype Response = Response
  { cid        :: Int
  , hyperdata  :: Hyperdata
  , category   :: Star
  , ngramCount :: Maybe Int
  , score      :: Maybe Int
  , title      :: String
  }


newtype Hyperdata = Hyperdata
  { title    :: String
  , source   :: String
  , pub_year :: Int
  }


instance decodeHyperdata :: DecodeJson Hyperdata where
  decodeJson json = do
    obj    <- decodeJson json
    pub_year <- obj .: "publication_year"
    source <- obj   .: "source"
    title  <- obj   .: "title"
    pure $ Hyperdata { title,source, pub_year}

instance decodeResponse :: DecodeJson Response where
  decodeJson json = do
    obj        <- decodeJson json
    category   <- obj .: "category"
    cid        <- obj .: "id"
    hyperdata  <- obj .: "hyperdata"
    ngramCount <- obj .: "ngramCount"
    score      <- obj .: "score"
    title      <- obj .: "title"
    --pure $ Response { category: decodeCategory category, cid, hyperdata, ngramCount, score, title }
    pure $ Response { category: decodeStar category, cid, hyperdata, ngramCount, score, title }

type LocalCategories = Map Int Category
type LocalUserScore  = Map Int Star
type Query = String

---------------------------------------------------------
sampleData' :: DocumentsView
sampleData' = DocumentsView { _id : 1
                            , url : ""
                            , date : 2010
                            , title : "title"
                            , source : "source"
                            , category : Star_1
                            , ngramCount : Just 1
                            , score: Just 1 }

sampleData :: Array DocumentsView
--sampleData = replicate 10 sampleData'
sampleData = map (\(Tuple t s) -> DocumentsView { _id : 1
                                                , url : ""
                                                , date : 2017
                                                , title: t
                                                , source: s
                                                , category : Star_1
                                                , ngramCount : Just 10
                                                , score: Just 1 }) sampleDocuments

sampleDocuments :: Array (Tuple String String)
sampleDocuments = [Tuple "Macroscopic dynamics of the fusion process" "Journal de Physique Lettres",Tuple "Effects of static and cyclic fatigue at high temperature upon reaction bonded silicon nitride" "Journal de Physique Colloques",Tuple "Reliability of metal/glass-ceramic junctions made by solid state bonding" "Journal de Physique Colloques",Tuple "High temperature mechanical properties and intergranular structure of sialons" "Journal de Physique Colloques",Tuple "SOLUTIONS OF THE LANDAU-VLASOV EQUATION IN NUCLEAR PHYSICS" "Journal de Physique Colloques",Tuple "A STUDY ON THE FUSION REACTION 139La + 12C AT 50 MeV/u WITH THE VUU EQUATION" "Journal de Physique Colloques",Tuple "Atomic structure of \"vitreous\" interfacial films in sialon" "Journal de Physique Colloques",Tuple "MICROSTRUCTURAL AND ANALYTICAL CHARACTERIZATION OF Al2O3/Al-Mg COMPOSITE INTERFACES" "Journal de Physique Colloques",Tuple "Development of oxidation resistant high temperature NbTiAl alloys and intermetallics" "Journal de Physique IV Colloque",Tuple "Determination of brazed joint constitutive law by inverse method" "Journal de Physique IV Colloque",Tuple "Two dimensional estimates from ocean SAR images" "Nonlinear Processes in Geophysics",Tuple "Comparison Between New Carbon Nanostructures Produced by Plasma with Industrial Carbon Black Grades" "Journal de Physique III",Tuple "<i>Letter to the Editor:</i> SCIPION, a new flexible ionospheric sounder in Senegal" "Annales Geophysicae",Tuple "Is reducibility in nuclear multifragmentation related to thermal scaling?" "Physics Letters B",Tuple "Independence of fragment charge distributions of the size of heavy multifragmenting sources" "Physics Letters B",Tuple "Hard photons and neutral pions as probes of hot and dense nuclear matter" "Nuclear Physics A",Tuple "Surveying the nuclear caloric curve" "Physics Letters B",Tuple "A hot expanding source in 50 A MeV Xe+Sn central reactions" "Physics Letters B"]
