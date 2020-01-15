module Gargantext.Components.Nodes.Corpus where

import Prelude ((<<<))
import Data.Argonaut (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Array (head)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, throwError)
import Effect.Exception (error)
import Reactix as R
import Reactix.DOM.HTML as H
import Gargantext.Prelude
import Gargantext.Components.MarkdownEditor (markdownEditor)
import Gargantext.Components.Node (NodePoly(..), HyperdataList)
import Gargantext.Types (NodeType(..), AffTableResult)
import Gargantext.Routes (SessionRoute(NodeAPI, Children))
import Gargantext.Sessions (Session, get)

type Props = ( nodeId :: Int )

corpusLayout :: Record Props -> R.Element
corpusLayout props = R.createElement corpusLayoutCpt props []

corpusLayoutCpt :: R.Component Props
corpusLayoutCpt = R.staticComponent "G.P.Corpus.corpusLayout" cpt
  where
    cpt {nodeId} _ =
      H.div {}
      [
        markdownEditor {md, nodeId}
        --H.iframe { src: gargMd , width: "100%", height: "100%", style: {"border-style": "none"}} []
      ]
    gargMd = "https://hackmd.iscpif.fr/g9Aah4iwQtCayIzsKQjA0Q#"
    md = "# Hello world"

newtype CorpusInfo =
  CorpusInfo
  { title        :: String
  , desc         :: String
  , query        :: String
  , authors      :: String
  , chart        :: (Maybe (Array Number))
  , totalRecords :: Int }

hyperdataDefault :: CorpusInfo
hyperdataDefault =
  CorpusInfo
  { title : "Default title"
  , desc  : " Default desc"
  , query : " Default Query"
  , authors : " Author(s): default"
  , chart   : Nothing
  , totalRecords : 0 }

corpusInfoDefault :: NodePoly CorpusInfo
corpusInfoDefault =
  NodePoly
  { id : 0
  , typename : 0
  , userId : 0
  , parentId : 0
  , name : "Default name"
  , date  : " Default date"
  , hyperdata : hyperdataDefault }

instance decodeCorpusInfo :: DecodeJson CorpusInfo where
  decodeJson json = do
    obj <- decodeJson json
    title <- obj .: "title"
    desc  <- obj .: "desc"
    query <- obj .: "query"
    authors <- obj .: "authors"
    chart   <- obj .:? "chart"
    let totalRecords = 47361 -- TODO
    pure $ CorpusInfo {title, desc, query, authors, chart, totalRecords}

type CorpusData = { corpusId :: Int
                  , corpusNode :: NodePoly CorpusInfo
                  , defaultListId :: Int}

loadCorpus :: { session :: Session, nodeId :: Int } -> Aff CorpusData
loadCorpus {session, nodeId: listId} = do
  -- fetch corpus via lists parentId
  (NodePoly {parentId: corpusId} :: NodePoly {}) <- get session nodePolyRoute
  corpusNode     <- get session $ corpusNodeRoute     corpusId ""
  defaultListIds <- (get session $ defaultListIdsRoute corpusId) :: forall a. DecodeJson a => AffTableResult (NodePoly a)
  case (head defaultListIds.docs :: Maybe (NodePoly HyperdataList)) of
    Just (NodePoly { id: defaultListId }) ->
      pure {corpusId, corpusNode, defaultListId}
    Nothing ->
      throwError $ error "Missing default list"
  where
    nodePolyRoute       = NodeAPI Corpus (Just listId) ""
    corpusNodeRoute     = NodeAPI Corpus <<< Just
    defaultListIdsRoute = Children NodeList 0 1 Nothing <<< Just
