module Gargantext.Components.DocsTable.Types where

import Gargantext.Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Gargantext.Components.Category.Types (Category, Star(..), decodeStar)
import Simple.JSON as JSON

data Action
  = MarkCategory Int Category

type DocumentsViewT =
  ( category   :: Star
  , date       :: Int
  , ngramCount :: Maybe Int
  , score      :: Maybe Int
  , source     :: Maybe String
  , title      :: String
  , url        :: String
  )
newtype DocumentsView
  = DocumentsView
    { _id        :: Int
    | DocumentsViewT
    }
derive instance Generic DocumentsView _
instance Eq DocumentsView where
  eq = genericEq

showSource :: Maybe String -> String
showSource s = fromMaybe "NOT FOUND" s

instance JSON.ReadForeign DocumentsView where
  readImpl f = do
    { id, category, date, ngramCount, score, source, title, url } :: { id :: Int | DocumentsViewT } <- JSON.readImpl f
    let source' = case source of
                    Just "NOT FOUND" -> Nothing
                    s                -> s
    pure $ DocumentsView { _id: id
                         , category
                         , date
                         , ngramCount
                         , score
                         , source: source'
                         , title
                         , url }
instance JSON.WriteForeign DocumentsView where
  writeImpl (DocumentsView { _id, category, date, ngramCount, score, source, title, url }) =
    JSON.writeImpl { id: _id
                   , category
                   , date
                   , ngramCount
                   , score
                   , source
                   , title
                   , url }

type ResponseT =
  ( hyperdata :: Hyperdata
  , ngramCount :: Maybe Int
  , score :: Maybe Int
  , title :: String )
newtype Response = Response
  { cid        :: Int
  , category :: Star
  | ResponseT
  }

instance JSON.ReadForeign Response where
  readImpl f = do
    { category, id, hyperdata, ngramCount, score, title } :: { category :: Int, id :: Int | ResponseT } <- JSON.readImpl f
    --pure $ Response { category: decodeCategory category, cid, hyperdata, ngramCount, score, title }
    pure $ Response { category: decodeStar category
                    , cid: id
                    , hyperdata
                    , ngramCount
                    , score
                    , title }


type HyperdataT =
  ( title :: String
  , source :: Maybe String )
newtype Hyperdata = Hyperdata
  { pub_year :: Int
  | HyperdataT
  }
derive instance Generic Hyperdata _

instance JSON.ReadForeign Hyperdata where
  readImpl f = do
    { publication_year, source, title} :: { publication_year :: Int | HyperdataT } <- JSON.readImpl f
    pure $ Hyperdata { pub_year: publication_year
                     , title
                     , source }

type LocalCategories = Map Int Category
type LocalUserScore  = Map Int Star
type Query = String
type Year = String

---------------------------------------------------------
sampleData' :: DocumentsView
sampleData' = DocumentsView { _id : 1
                            , url : ""
                            , date : 2010
                            , title : "title"
                            , source : Just "source"
                            , category : Star_1
                            , ngramCount : Just 1
                            , score: Just 1 }

sampleData :: Array DocumentsView
--sampleData = replicate 10 sampleData'
sampleData = map (\(Tuple t s) -> DocumentsView { _id : 1
                                                , url : ""
                                                , date : 2017
                                                , title: t
                                                , source: Just s
                                                , category : Star_1
                                                , ngramCount : Just 10
                                                , score: Just 1 }) sampleDocuments

sampleDocuments :: Array (Tuple String String)
sampleDocuments = [Tuple "Macroscopic dynamics of the fusion process" "Journal de Physique Lettres",Tuple "Effects of static and cyclic fatigue at high temperature upon reaction bonded silicon nitride" "Journal de Physique Colloques",Tuple "Reliability of metal/glass-ceramic junctions made by solid state bonding" "Journal de Physique Colloques",Tuple "High temperature mechanical properties and intergranular structure of sialons" "Journal de Physique Colloques",Tuple "SOLUTIONS OF THE LANDAU-VLASOV EQUATION IN NUCLEAR PHYSICS" "Journal de Physique Colloques",Tuple "A STUDY ON THE FUSION REACTION 139La + 12C AT 50 MeV/u WITH THE VUU EQUATION" "Journal de Physique Colloques",Tuple "Atomic structure of \"vitreous\" interfacial films in sialon" "Journal de Physique Colloques",Tuple "MICROSTRUCTURAL AND ANALYTICAL CHARACTERIZATION OF Al2O3/Al-Mg COMPOSITE INTERFACES" "Journal de Physique Colloques",Tuple "Development of oxidation resistant high temperature NbTiAl alloys and intermetallics" "Journal de Physique IV Colloque",Tuple "Determination of brazed joint constitutive law by inverse method" "Journal de Physique IV Colloque",Tuple "Two dimensional estimates from ocean SAR images" "Nonlinear Processes in Geophysics",Tuple "Comparison Between New Carbon Nanostructures Produced by Plasma with Industrial Carbon Black Grades" "Journal de Physique III",Tuple "<i>Letter to the Editor:</i> SCIPION, a new flexible ionospheric sounder in Senegal" "Annales Geophysicae",Tuple "Is reducibility in nuclear multifragmentation related to thermal scaling?" "Physics Letters B",Tuple "Independence of fragment charge distributions of the size of heavy multifragmenting sources" "Physics Letters B",Tuple "Hard photons and neutral pions as probes of hot and dense nuclear matter" "Nuclear Physics A",Tuple "Surveying the nuclear caloric curve" "Physics Letters B",Tuple "A hot expanding source in 50 A MeV Xe+Sn central reactions" "Physics Letters B"]
