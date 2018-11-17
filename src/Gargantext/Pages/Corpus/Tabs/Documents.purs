module Gargantext.Pages.Corpus.Tabs.Documents where

import Gargantext.Prelude

import Affjax (defaultRequest, request)
import Affjax.RequestBody (RequestBody(..))
import Affjax.ResponseFormat (printResponseFormatError)
import Affjax.ResponseFormat as ResponseFormat
import Control.Monad.Cont.Trans (lift)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonEmptyObject, (.?), (:=), (~>))
import Data.Array (take, drop)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HTTP.Method (Method(..))
import Data.List (List)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Gargantext.Components.Charts.Options.ECharts (chart)
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.Table as T
import Gargantext.Config (NodeType(..), TabType(..), toUrl, End(..), OrderBy(..))
import Gargantext.Config.REST (get, post)
import Gargantext.Pages.Corpus.Dashboard (globalPublis)
import Gargantext.Pages.Corpus.Tabs.Types (CorpusInfo(..), Props)
import Gargantext.Utils.DecodeMaybe ((.|))
import React.DOM (a, br', button, div, i, input, p, text)
import React.DOM.Props (_type, className, href, name, onClick, placeholder, style, value)
import Thermite (PerformAction, Render, Spec, defaultPerformAction, modifyState, simpleSpec)
------------------------------------------------------------------------
-- TODO: Pagination Details are not available from the BackEnd
-- TODO: Search is pending
-- TODO: Fav is pending
-- TODO: Sort is Pending
-- TODO: Filter is Pending
-- TODO: When a pagination link is clicked, reload data. 

type State = 
  { documents :: DocumentsView
  , deleteRows :: Boolean
  , deleteRowId :: Int
  }



data Action
  = SendFavorites Int
  | DeleteDocuments Int
  | Trash

performAction :: PerformAction State Props Action
performAction (SendFavorites nid) _ _ = void $ do
  
  s' <- lift $ favorites nid (FavoriteQuery {favorites : [nid]})
  case s' of
    Left err -> do
      _ <- liftEffect $ log err
      modifyState identity
    Right d -> modifyState identity
  
performAction (DeleteDocuments nid) _ _ = void $ do
  _ <- liftEffect $ log $ show nid
  modifyState \state -> state {deleteRowId = nid, deleteRows = true}
    
performAction (Trash) _ state  = void $ do
  s' <- lift $ deleteDocuments state.deleteRowId (DeleteDocumentQuery {documents : [state.deleteRowId]})
  case s' of
    Left err -> do
      _ <- liftEffect $ log err
      modifyState identity
    Right d -> modifyState identity

    
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
filterSpec :: Spec State Props Action
filterSpec = simpleSpec defaultPerformAction render
  where
    render d p s c = [div [] [ text "    Filter "
                     , input []
                     ]]

-- | Main layout of the Documents Tab of a Corpus
layoutDocview :: Spec State Props Action
layoutDocview = simpleSpec performAction render
  where
    render :: Render State Props Action
    render dispatch {path: nodeId, loaded} _ s =
      [ p [] []
      , div [ style {textAlign : "center"}] [input [placeholder "Filter here"]]
      , br'
      , div [className "container1"]
        [ div [className "row"]
          [ chart globalPublis
          , div [className "col-md-12"]
            [ T.tableElt
                { loadRows
                , container: T.defaultContainer { title: "Documents" }
                , colNames:
                    T.ColumnName <$>
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
          , div [className "col-md-12"]
             [ button [style {backgroundColor: "peru", padding : "9px", color : "white", border : "white", float: "right"}
                      , onClick $ (\_ -> dispatch $ Trash )
                      ]
               [  i [className "glyphitem glyphicon glyphicon-trash", style {marginRight : "9px"}] []
               ,  text "Trash it !"
               
               ]
             
             ]
          ]
        ]
      ]
      where
        loadRows {offset, limit, orderBy} = do
          _ <- logs "loading documents page"
          res <- loadPage {nodeId,offset,limit,orderBy}
          _ <- logs "OK: loading page documents."
          pure $
            (\(DocumentsView r) ->
                { row:
                    [ div []
                      [ a [className $ fa r.fav <> "fa-star" ,onClick $ (\_-> dispatch $ (SendFavorites r._id))] []
                      ]
                    -- TODO show date: Year-Month-Day only
                    , text r.date 
                    , a [ href (toUrl Front Url_Document r._id) ] [ text r.title ]
                    , text r.source
                    , input [ _type "checkbox", onClick $ (\_ -> dispatch $ (DeleteDocuments r._id))]
                    ]
                , delete: true
                }) <$> res
    fa true  = "fas "
    fa false = "far "

mock :: Boolean
mock = false

loadPage :: {nodeId :: Int, limit :: Int, offset :: Int, orderBy :: T.OrderBy} -> Aff (Array DocumentsView)
loadPage {nodeId, limit, offset, orderBy} = do
  logs "loading documents page: loadPage with Offset and limit"
  --res <- get $ toUrl Back (Children Url_Document offset limit) nodeId
  res <- get $ toUrl Back (Tab TabDocs offset limit (convOrderBy <$> orderBy)) nodeId
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
    convOrderBy (T.ASC  (T.ColumnName "Date")) = DateAsc
    convOrderBy (T.DESC (T.ColumnName "Date")) = DateDesc
    convOrderBy (T.ASC  (T.ColumnName "Title")) = TitleAsc
    convOrderBy (T.DESC (T.ColumnName "Title")) = TitleDesc

    convOrderBy _ = DateAsc -- TODO


---------------------------------------------------------
sampleData' :: DocumentsView
sampleData' = DocumentsView {_id : 1, url : "", date : "date3", title : "title", source : "source", fav : false, ngramCount : 1}

sampleData :: Array DocumentsView
--sampleData = replicate 10 sampleData'
sampleData = map (\(Tuple t s) -> DocumentsView {_id : 1, url : "", date : "2017", title: t, source: s, fav : false, ngramCount : 10}) sampleDocuments

sampleDocuments :: Array (Tuple String String)
sampleDocuments = [Tuple "Macroscopic dynamics of the fusion process" "Journal de Physique Lettres",Tuple "Effects of static and cyclic fatigue at high temperature upon reaction bonded silicon nitride" "Journal de Physique Colloques",Tuple "Reliability of metal/glass-ceramic junctions made by solid state bonding" "Journal de Physique Colloques",Tuple "High temperature mechanical properties and intergranular structure of sialons" "Journal de Physique Colloques",Tuple "SOLUTIONS OF THE LANDAU-VLASOV EQUATION IN NUCLEAR PHYSICS" "Journal de Physique Colloques",Tuple "A STUDY ON THE FUSION REACTION 139La + 12C AT 50 MeV/u WITH THE VUU EQUATION" "Journal de Physique Colloques",Tuple "Atomic structure of \"vitreous\" interfacial films in sialon" "Journal de Physique Colloques",Tuple "MICROSTRUCTURAL AND ANALYTICAL CHARACTERIZATION OF Al2O3/Al-Mg COMPOSITE INTERFACES" "Journal de Physique Colloques",Tuple "Development of oxidation resistant high temperature NbTiAl alloys and intermetallics" "Journal de Physique IV Colloque",Tuple "Determination of brazed joint constitutive law by inverse method" "Journal de Physique IV Colloque",Tuple "Two dimensional estimates from ocean SAR images" "Nonlinear Processes in Geophysics",Tuple "Comparison Between New Carbon Nanostructures Produced by Plasma with Industrial Carbon Black Grades" "Journal de Physique III",Tuple "<i>Letter to the Editor:</i> SCIPION, a new flexible ionospheric sounder in Senegal" "Annales Geophysicae",Tuple "Is reducibility in nuclear multifragmentation related to thermal scaling?" "Physics Letters B",Tuple "Independence of fragment charge distributions of the size of heavy multifragmenting sources" "Physics Letters B",Tuple "Hard photons and neutral pions as probes of hot and dense nuclear matter" "Nuclear Physics A",Tuple "Surveying the nuclear caloric curve" "Physics Letters B",Tuple "A hot expanding source in 50 A MeV Xe+Sn central reactions" "Physics Letters B"]

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



newtype FavoriteQuery = FavoriteQuery 
                        { favorites :: Array Int
                        }

instance encodeJsonFQuery :: EncodeJson FavoriteQuery where
  encodeJson (FavoriteQuery post)
     = "favorites" := post.favorites
       ~> jsonEmptyObject

newtype DeleteDocumentQuery = DeleteDocumentQuery
  {
    documents :: Array Int
  }


instance encodeJsonDDQuery :: EncodeJson DeleteDocumentQuery where
  encodeJson (DeleteDocumentQuery post)
     = "documents" := post.documents
       ~> jsonEmptyObject


favorites :: Int -> FavoriteQuery -> Aff (Either String (Array Int))
favorites nodeId reqbody= do
  res <- request $ defaultRequest
         { url = "http://localhost:8008/api/v1.0/node/"<>show nodeId <>"/favorites"
         , responseFormat = ResponseFormat.json
         , method = Left PUT
         , headers = []
         , content = Just $ Json $ encodeJson reqbody
         }
  case res.body of
    Left err -> do
      _ <- liftEffect $ log $ printResponseFormatError err
      pure $ Left $ printResponseFormatError err
    Right json -> do
      let obj = decodeJson json
      pure obj

deleteFavorites :: Int -> FavoriteQuery -> Aff (Either String (Array Int))
deleteFavorites nodeId reqbody= do
  res <- request $ defaultRequest
         { url = "http://localhost:8008/api/v1.0/node/"<>show nodeId<>"/favorites"
         , responseFormat = ResponseFormat.json
         , method = Left DELETE
         , headers = []
         , content = Just $ Json $ encodeJson reqbody
         }
  case res.body of
    Left err -> do
      _ <- liftEffect $ log $ printResponseFormatError err
      pure $ Left $ printResponseFormatError err
    Right json -> do
      let obj = decodeJson json
      pure obj



deleteDocuments :: Int -> DeleteDocumentQuery -> Aff (Either String Unit)
deleteDocuments ddid reqbody= do
  res <- request $ defaultRequest
         { url = "http://localhost:8008/api/v1.0/annuaire/"<>show ddid<>"/documents"
         , responseFormat = ResponseFormat.json
         , method = Left DELETE
         , headers = []
         , content = Just $ Json $ encodeJson reqbody
         }
  case res.body of
    Left err -> do
      _ <- liftEffect $ log $ printResponseFormatError err
      pure $ Left $ printResponseFormatError err
    Right json -> do
      -- let obj = decodeJson json
      pure $ Right unit
