module Gargantext.Pages.Corpus.Tabs.Documents where

import Data.Array (take, drop)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, jsonEmptyObject, (.?), (:=), (~>))

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import React.DOM (a, br', div, input, p, text)
import React.DOM.Props (_type, className, href)
import Thermite (Render, Spec, defaultPerformAction, simpleSpec)
------------------------------------------------------------------------
import Gargantext.Prelude
import Gargantext.Config (NodeType(..), TabType(..), toUrl, End(..))
import Gargantext.Config.REST (get, post)
import Gargantext.Utils.DecodeMaybe ((.|))
import Gargantext.Components.Charts.Options.ECharts (chart)
import Gargantext.Components.Table as T
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Pages.Corpus.Tabs.Types
import Gargantext.Pages.Corpus.Dashboard (globalPublis)
------------------------------------------------------------------------
-- TODO: Pagination Details are not available from the BackEnd
-- TODO: Search is pending
-- TODO: Fav is pending
-- TODO: Sort is Pending
-- TODO: Filter is Pending
-- TODO: When a pagination link is clicked, reload data. 

type State = {}

type Action = Void

newtype DocumentsView
  = DocumentsView
    { _id    :: Int
    , url    :: String
    , date   :: String
    , title  :: String
    , source :: String
    , fav    :: Boolean
    , ngramCount :: Int
    }


derive instance genericCorpus :: Generic DocumentsView _

instance showCorpus :: Show DocumentsView where
  show = genericShow


newtype Response = Response
  { cid        :: Int
  , created    :: String
  , hyperdata  :: Hyperdata
  , favorite   :: Boolean
  , ngramCount :: Int
  }


newtype Hyperdata = Hyperdata
  { title  :: String
  , source :: String
  }

--instance decodeHyperdata :: DecodeJson Hyperdata where
--  decodeJson json = do
--    obj    <- decodeJson json
--    title  <- obj .? "title"
--    source <- obj .? "source"
--    pure $ Hyperdata { title,source }
--instance decodeResponse :: DecodeJson Response where
--  decodeJson json = do
--    obj        <- decodeJson json
--    cid        <- obj .? "id"
--    created    <- obj .? "created"
--    favorite   <- obj .? "favorite"
--    ngramCount <- obj .? "ngramCount"
--    hyperdata  <- obj .? "hyperdata"
--    pure $ Response { cid, created, favorite, ngramCount, hyperdata }


instance decodeHyperdata :: DecodeJson Hyperdata where
  decodeJson json = do
    obj    <- decodeJson json
    title  <- obj .| "title"
    source <- obj .| "source"
    pure $ Hyperdata { title,source }

instance decodeResponse :: DecodeJson Response where
  decodeJson json = do
    obj        <- decodeJson json
    cid        <- obj .? "id"
    created    <- pure "2018"
    --created    <- obj .? "date"
    favorite   <- pure true
    ngramCount <- obj .? "id"
    hyperdata  <- obj .? "hyperdata"
    pure $ Response { cid, created, favorite, ngramCount, hyperdata }



-- | Filter
filterSpec :: Spec State {} Action
filterSpec = simpleSpec defaultPerformAction render
  where
    render d p s c = [div [] [ text "    Filter "
                     , input []
                     ]]

-- | Main layout of the Documents Tab of a Corpus
layoutDocview :: Spec State Props Action
layoutDocview = simpleSpec absurd render
  where
    render :: Render State Props Action
    render dispatch {path, loaded} _ _ =
      [ div [className "container1"]
        [ div [className "row"]
          [ chart globalPublis
          , div [className "col-md-12"]
            [ p [] []
            , div [] [ text "    Filter ", input []]
            , br'
            , T.tableElt
                { loadRows
                , title: "Documents"
                , colNames:
                    [ ""
                    , "Date"
                    , "Title"
                    , "Source"
                    , "Delete"
                    ]
                , totalRecords: maybe 47361 -- TODO
                                  identity
                                  ((\(NodePoly n) -> n.hyperdata)
                                   >>>
                                   (\(CorpusInfo c) -> c.totalRecords)
                                  <$> loaded)
                }
            ]
          ]
        ]
      ]
      where
        loadRows {offset, limit} = do
          _ <- logs "loading documents page"
          res <- loadPage {nodeId: path,offset,limit}
          _ <- logs "OK: loading page documents."
          pure $
            (\(DocumentsView r) ->
                { row:
                    [ div [className $ fa r.fav <> "fa-star"] []
                    -- TODO show date: Year-Month-Day only
                    , text r.date
                    , a [ href (toUrl Front Url_Document r._id) ] [ text r.title ]
                    , text r.source
                    , input [ _type "checkbox"]
                    ]
                , delete: false
                }) <$> res
    fa true  = "fas "
    fa false = "far "

mock :: Boolean
mock = false

loadPage :: {nodeId :: Int, limit :: Int, offset :: Int} -> Aff (Array DocumentsView)
loadPage {nodeId, limit, offset} = do
  logs "loading documents page: loadPage with Offset and limit"
  --res <- get $ toUrl Back (Children Url_Document offset limit) nodeId
  res <- get $ toUrl Back (Tab TabDocs offset limit ) nodeId
  let docs = res2corpus <$> res
  _ <- logs "Ok: loading page documents"
  _ <- logs $ map show docs
  pure $
    if mock then take limit $ drop offset sampleData else
    docs
  where
    res2corpus :: Response -> DocumentsView
    res2corpus (Response r) =
      DocumentsView { _id : r.cid
      , url    : ""
      , date   :  r.created
      , title  : (\(Hyperdata hr) -> hr.title) r.hyperdata
      , source : (\(Hyperdata hr) -> hr.source) r.hyperdata
      , fav    : r.favorite
      , ngramCount : r.ngramCount
     }


---------------------------------------------------------
sampleData' :: DocumentsView
sampleData' = DocumentsView {_id : 1, url : "", date : "date3", title : "title", source : "source", fav : false, ngramCount : 1}

sampleData :: Array DocumentsView
--sampleData = replicate 10 sampleData'
sampleData = map (\(Tuple t s) -> DocumentsView {_id : 1, url : "", date : "2017", title: t, source: s, fav : false, ngramCount : 10}) sampleDocuments

sampleDocuments :: Array (Tuple String String)
sampleDocuments = [Tuple "Macroscopic dynamics of the fusion process" "Journal de Physique Lettres",Tuple "Effects of static and cyclic fatigue at high temperature upon reaction bonded silicon nitride" "Journal de Physique Colloques",Tuple "Reliability of metal/glass-ceramic junctions made by solid state bonding" "Journal de Physique Colloques",Tuple "High temperature mechanical properties and intergranular structure of sialons" "Journal de Physique Colloques",Tuple "SOLUTIONS OF THE LANDAU-VLASOV EQUATION IN NUCLEAR PHYSICS" "Journal de Physique Colloques",Tuple "A STUDY ON THE FUSION REACTION 139La + 12C AT 50 MeV/u WITH THE VUU EQUATION" "Journal de Physique Colloques",Tuple "Atomic structure of \"vitreous\" interfacial films in sialon" "Journal de Physique Colloques",Tuple "MICROSTRUCTURAL AND ANALYTICAL CHARACTERIZATION OF Al2O3/Al-Mg COMPOSITE INTERFACES" "Journal de Physique Colloques",Tuple "Development of oxidation resistant high temperature NbTiAl alloys and intermetallics" "Journal de Physique IV Colloque",Tuple "Determination of brazed joint constitutive law by inverse method" "Journal de Physique IV Colloque",Tuple "Two dimensional estimates from ocean SAR images" "Nonlinear Processes in Geophysics",Tuple "Comparison Between New Carbon Nanostructures Produced by Plasma with Industrial Carbon Black Grades" "Journal de Physique III",Tuple "<i>Letter to the Editor:</i> SCIPION, a new flexible ionospheric sounder in Senegal" "Annales Geophysicae",Tuple "Is reducibility in nuclear multifragmentation related to thermal scaling?" "Physics Letters B",Tuple "Independence of fragment charge distributions of the size of heavy multifragmenting sources" "Physics Letters B",Tuple "Hard photons and neutral pions as probes of hot and dense nuclear matter" "Nuclear Physics A",Tuple "Surveying the nuclear caloric curve" "Physics Letters B",Tuple "A hot expanding source in 50 A MeV Xe+Sn central reactions" "Physics Letters B"]

initialState :: State
initialState = {}

newtype SearchQuery = SearchQuery
  {
    query :: Array String
  , parent_id :: Int
  }


instance encodeJsonSQuery :: EncodeJson SearchQuery where
  encodeJson (SearchQuery post)
     = "query" := post.query
    ~> "parent_id" := post.parent_id
    ~> jsonEmptyObject



searchResults :: SearchQuery -> Aff Int
searchResults squery = post "http://localhost:8008/count" unit
  -- TODO
