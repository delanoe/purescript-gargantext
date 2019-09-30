module Gargantext.Pages.Corpus.Document where

import Prelude (class Show, bind, identity, mempty, pure, ($), (<<<))
import Data.Argonaut (class DecodeJson, decodeJson, (.:), (.??))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Aff)
import React (ReactClass, Children)
import React.DOM (div, h4, li, p, span, text, ul)
import React.DOM.Props (className)
import Reactix as R
import Thermite (PerformAction, Render, Spec, simpleSpec, createClass)

import Gargantext.Config.REST     (get)
import Gargantext.Components.AutoUpdate (autoUpdateElt)
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.NgramsTable.Core
  ( CoreState, NgramsPatch(..), NgramsTerm, Replace, Versioned(..)
  , VersionedNgramsTable, addNewNgram, applyNgramsTablePatch, commitPatch
  , loadNgramsTable, replace, singletonNgramsTablePatch )
import Gargantext.Components.Annotation.AnnotatedField as AnnotatedField
import Gargantext.Ends (url)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session)
import Gargantext.Types (CTabNgramType(..), NodeType(..), TabSubType(..), TabType(..), TermList)
import Gargantext.Utils.Reactix as R2

type DocPath = { nodeId :: Int, listIds :: Array Int, corpusId :: Maybe Int, tabType :: TabType }

type NodeDocument = NodePoly Document

type LoadedData =
  { document    :: NodeDocument
  , ngramsTable :: VersionedNgramsTable
  }

type Props =
  { loaded :: LoadedData
  , path   :: DocPath
  , session   :: Session
  }

-- This is a subpart of NgramsTable.State.
type State = CoreState ()

initialState
  :: forall props others
  .  { loaded :: { ngramsTable :: VersionedNgramsTable | others }
     | props }
  -> State
initialState {loaded: {ngramsTable: Versioned {version}}} =
  { ngramsTablePatch: mempty
  , ngramsVersion:    version
  }

-- This is a subset of NgramsTable.Action.
data Action
  = SetTermListItem NgramsTerm (Replace TermList)
  | AddNewNgram NgramsTerm TermList
  | Refresh

newtype Status = Status { failed    :: Int
                        , succeeded :: Int
                        , remaining :: Int
                        }

newtype DocumentV3 =
  DocumentV3 { abstract           :: Maybe String
             , authors            :: Maybe String
             --, error              :: Maybe String
             , language_iso2      :: Maybe String
             , language_iso3      :: Maybe String
             , language_name      :: Maybe String
             , publication_date   :: Maybe String
             , publication_day    :: Maybe Int
             , publication_hour   :: Maybe Int
             , publication_minute :: Maybe Int
             , publication_month  :: Maybe Int
             , publication_second :: Maybe Int
             , publication_year   :: Maybe Int
             , realdate_full_     :: Maybe String
             , source             :: Maybe String
             , statuses           :: Maybe (Array Status)
             , title              :: Maybe String
             }

defaultNodeDocumentV3 :: NodePoly DocumentV3
defaultNodeDocumentV3 =
  NodePoly { id : 0
           , typename : 0
           , userId   : 0
           , parentId : 0
           , name     : "Default name"
           , date     : "Default date"
           , hyperdata : defaultDocumentV3
         }

defaultDocumentV3 :: DocumentV3
defaultDocumentV3 =
  DocumentV3 { abstract           : Nothing
             , authors            : Nothing
             --, error              : Nothing
             , language_iso2      : Nothing
             , language_iso3      : Nothing
             , language_name      : Nothing
             , publication_date   : Nothing
             , publication_day    : Nothing
             , publication_hour   : Nothing
             , publication_minute : Nothing
             , publication_month  : Nothing
             , publication_second : Nothing
             , publication_year   : Nothing
             , realdate_full_     : Nothing
             , source             : Nothing
             , statuses           : Nothing
             , title              : Nothing
             }

data Document
  = Document
    { abstract           :: Maybe String
    , authors            :: Maybe String
    , bdd                :: Maybe String
    , doi                :: Maybe String
    , language_iso2      :: Maybe String
    -- , page               :: Maybe Int
    , publication_date   :: Maybe String
    --, publication_second :: Maybe Int
    --, publication_minute :: Maybe Int
    --, publication_hour   :: Maybe Int
    , publication_day    :: Maybe Int
    , publication_month  :: Maybe Int
    , publication_year   :: Maybe Int
    , source             :: Maybe String
    , institutes         :: Maybe String
    , title              :: Maybe String
    , uniqId             :: Maybe String
    --, url                :: Maybe String
    --, text               :: Maybe String
    }

defaultNodeDocument :: NodeDocument
defaultNodeDocument =
  NodePoly { id : 0
           , typename : 0
           , userId   : 0
           , parentId : 0
           , name     : "Default name"
           , date     : "Default date"
           , hyperdata : defaultDocument
         }

-- TODO: BUG if DOI does not exist, page is not shown
defaultDocument :: Document
defaultDocument =
  Document { abstract           : Nothing
           , authors            : Nothing
           , bdd                : Nothing
           , doi                : Nothing
           , language_iso2      : Nothing
           --, page               : Nothing
           , publication_date   : Nothing
           --, publication_second : Nothing
           --, publication_minute : Nothing
           --, publication_hour   : Nothing
           , publication_day    : Nothing
           , publication_month  : Nothing
           , publication_year   : Nothing
           , source             : Nothing
           , institutes         : Nothing
           , title              : Nothing
           , uniqId             : Nothing
           --, url                : Nothing
           --, text               : Nothing
           }

derive instance genericDocument   :: Generic Document   _
derive instance genericDocumentV3 :: Generic DocumentV3 _
derive instance genericStatus     :: Generic Status     _

instance showDocument :: Show Document where
  show = genericShow

instance showDocumentV3 :: Show DocumentV3 where
  show = genericShow

instance showStatus :: Show Status where
  show = genericShow

instance decodeStatus :: DecodeJson Status
  where
    decodeJson json = do
      obj <- decodeJson json
      failed <- obj .: "failed"
      succeeded <- obj .: "succeeded"
      remaining <- obj .: "remaining"
      pure $ Status {failed, succeeded, remaining}


instance decodeDocumentV3 :: DecodeJson DocumentV3
  where
    decodeJson json = do
      obj <- decodeJson json
      abstract <- obj .?? "abstract"
      authors  <- obj .: "authors"
      --error    <- obj .: "error"
      language_iso2 <- obj .: "language_iso2"
      language_iso3 <- obj .: "language_iso3"
      language_name <- obj .: "language_name"
      publication_date   <- obj .: "publication_date"
      publication_day    <- obj .: "publication_day"
      publication_hour   <- obj .: "publication_hour"
      publication_minute <- obj .: "publication_minute"
      publication_month  <- obj .: "publication_month"
      publication_second <- obj .: "publication_second"
      publication_year   <- obj .: "publication_year"
      realdate_full_     <- obj .: "realdate_full_"
      source   <- obj .: "source"
      statuses <- obj .: "statuses"
      title    <- obj .: "title"
      pure $ DocumentV3 { abstract
                        , authors
                        --, error
                        , language_iso2
                        , language_iso3
                        , language_name
                        , publication_date
                        , publication_day
                        , publication_hour
                        , publication_minute
                        , publication_month
                        , publication_second
                        , publication_year
                        , realdate_full_
                        , source
                        , statuses
                        , title
                        }

instance decodeDocument :: DecodeJson Document
  where
    decodeJson json = do
      obj <- decodeJson json
      abstract <- obj .?? "abstract"
      authors  <- obj .?? "authors"
      bdd      <- obj .?? "bdd"
      doi      <- obj .?? "doi"
      language_iso2 <- obj .?? "language_iso2"
      -- page          <- obj .?? "page"
      publication_date   <- obj .?? "publication_date"
      --publication_second <- obj .?? "publication_second"
      --publication_minute <- obj .?? "publication_minute"
      --publication_hour   <- obj .?? "publication_hour"
      publication_day    <- obj .?? "publication_day"
      publication_month  <- obj .?? "publication_month"
      publication_year   <- obj .?? "publication_year"
      source             <- obj .?? "sources"
      institutes         <- obj .?? "institutes"
      title              <- obj .?? "title"
      uniqId             <- obj .?? "uniqId"
      --url                <- obj .: "url"
      --text               <- obj .: "text"
      pure $ Document { abstract
                      , authors
                      , bdd
                      , doi
                      , language_iso2
                      -- , page
                      , publication_date
                      --, publication_second
                      --, publication_minute
                      --, publication_hour
                      , publication_day
                      , publication_month
                      , publication_year
                      , source
                      , institutes
                      , title
                      , uniqId
                      --, url
                      --, text
                      }

docViewSpec :: Spec State Props Action
docViewSpec = simpleSpec performAction render
  where
    performAction :: PerformAction State Props Action
    performAction Refresh {path: {nodeId, listIds, tabType}, session} {ngramsVersion} = do
        commitPatch session {nodeId, listIds, tabType} (Versioned {version: ngramsVersion, data: mempty})
    performAction (SetTermListItem n pl) {path: {nodeId, listIds, tabType}, session} {ngramsVersion} =
        commitPatch session {nodeId, listIds, tabType} (Versioned {version: ngramsVersion, data: pt})
      where
        pe = NgramsPatch { patch_list: pl, patch_children: mempty }
        pt = singletonNgramsTablePatch CTabTerms n pe
    performAction (AddNewNgram ngram termList) {path: {nodeId, listIds, tabType},session} {ngramsVersion} =
        commitPatch session {nodeId, listIds, tabType} (Versioned {version: ngramsVersion, data: pt})
      where
        pt = addNewNgram CTabTerms ngram termList

    render :: Render State Props Action
    render dispatch { loaded: { ngramsTable: Versioned { data: initTable }, document } }
                    { ngramsTablePatch }
                    _reactChildren =
      [ autoUpdateElt { duration: 3000
                      , effect:   dispatch Refresh
                      }
      , div [className "container1"]
        [
          div [className "row"]
          [
            div [className "col-md-8"]
            [ h4 [] [annotate doc.title]
            , ul [className "list-group"]
              [ li' [ span [] [text' doc.source]
                    , badge "source"
                    ]
              -- TODO add href to /author/ if author present in
              , li' [ span [] [text' doc.authors]
                    , badge "authors"
                    ]
              , li' [ span [] [text' doc.publication_date]
                    , badge "date"
                    ]
              ]
            , badge "abstract"
            , annotate doc.abstract
            , div [className "jumbotron"]
              [ p [] [text "Empty Full Text"]
              ]
            ]
          ]
        ]
      ]
        where
          ngramsTable = applyNgramsTablePatch ngramsTablePatch initTable
          setTermList ngram Nothing        newList = dispatch $ AddNewNgram ngram newList
          setTermList ngram (Just oldList) newList = dispatch $ SetTermListItem ngram (replace oldList newList)
          annotate text = R2.scuff $ AnnotatedField.annotatedField { ngrams: ngramsTable, setTermList, text }
          li' = li [className "list-group-item justify-content-between"]
          text' x = text $ maybe "Nothing" identity x
          badge s = span [className "badge badge-default badge-pill"] [text s]
          NodePoly {hyperdata : Document doc} = document

docViewClass
  :: ReactClass
     { session     :: Session
     , children :: Children
     , loaded   :: LoadedData
     , path     :: DocPath }
docViewClass = createClass "DocumentView" docViewSpec initialState

type LayoutProps = ( session :: Session, nodeId :: Int, listId :: Int, corpusId :: Maybe Int )

documentLayout :: Record LayoutProps -> R.Element
documentLayout props = R.createElement documentLayoutCpt props []

documentLayoutCpt :: R.Component LayoutProps
documentLayoutCpt = R.hooksComponent "G.P.Corpus.Document.documentLayout" cpt
  where
    cpt {session, nodeId, listId, corpusId} _ = do
      useLoader path (loadData session) $ \loaded ->
        R2.createElement' docViewClass {session, path, loaded} []
      where
        tabType = TabDocument (TabNgramType CTabTerms)
        path = {nodeId, listIds: [listId], corpusId, tabType}

------------------------------------------------------------------------

loadDocument :: Session -> Int -> Aff NodeDocument
loadDocument session = get <<< url session <<< NodeAPI Node <<< Just

loadData :: Session -> DocPath -> Aff LoadedData
loadData session {nodeId, listIds, tabType} = do
  document <- loadDocument session nodeId
  ngramsTable <- loadNgramsTable session
    { session
    , nodeId
    , listIds: listIds
    , params: { offset : 0, limit : 100, orderBy: Nothing}
    , tabType
    , searchQuery : ""
    , termListFilter : Nothing
    , termSizeFilter : Nothing
    }
  pure {document, ngramsTable}
