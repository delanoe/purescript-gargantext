module Gargantext.Components.Nodes.Corpus.Document where

import Prelude (class Show, bind, mempty, pure, show, ($), (<>), Unit)
import Data.Argonaut (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Components.AutoUpdate ( autoUpdate)
import Gargantext.Components.Search (SearchType(..))
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.NgramsTable.Core
  ( CoreState, NgramsPatch(..), NgramsTerm, Replace, Versioned(..)
  , VersionedNgramsTable, addNewNgram, applyNgramsTablePatch, commitPatchR
  , loadNgramsTable, replace, singletonNgramsTablePatch, syncPatchesR )
import Gargantext.Components.Annotation.AnnotatedField as AnnotatedField
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, get, sessionId)
import Gargantext.Types (CTabNgramType(..), NodeType(..), TabSubType(..), TabType(..), TermList, ScoreType(..))
import Gargantext.Utils as U
import Gargantext.Utils.Reactix as R2

type DocPath =
  {
    corpusId :: Maybe Int
  , listIds  :: Array Int
  , nodeId   :: Int
  , session  :: Session
  , tabType  :: TabType
  }

type NodeDocument = NodePoly Document

type LoadedData =
  { document    :: NodeDocument
  , ngramsTable :: VersionedNgramsTable
  }

type Props = (
    loaded :: LoadedData
  , path   :: DocPath
  )

-- This is a subpart of NgramsTable.State.
type State = CoreState ()

initialState
  :: forall props others
  .  { loaded :: { ngramsTable :: VersionedNgramsTable | others }
     | props }
  -> State
initialState {loaded: {ngramsTable: Versioned {version}}} =
  { ngramsLocalPatch: mempty
  , ngramsStagePatch: mempty
  , ngramsValidPatch: mempty
  , ngramsVersion:    version
  }

-- This is a subset of NgramsTable.Action.
data Action
  = SetTermListItem NgramsTerm (Replace TermList)
  | AddNewNgram NgramsTerm TermList
  | Synchronize

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

publicationDate :: Document -> String
publicationDate (Document doc@{publication_year: Nothing}) = ""
publicationDate (Document doc@{publication_year: Just py, publication_month: Nothing}) = U.zeroPad 2 py
publicationDate (Document doc@{publication_year: Just py, publication_month: Just pm, publication_day: Nothing}) = (U.zeroPad 2 py) <> "-" <> (U.zeroPad 2 pm)
publicationDate (Document doc@{publication_year: Just py, publication_month: Just pm, publication_day: Just pd}) = (U.zeroPad 2 py) <> "-" <> (U.zeroPad 2 pm) <> "-" <> (U.zeroPad 2 pd)

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
      abstract <- obj .:? "abstract"
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
      abstract <- obj .:? "abstract"
      authors  <- obj .:? "authors"
      bdd      <- obj .:? "bdd"
      doi      <- obj .:? "doi"
      language_iso2 <- obj .:? "language_iso2"
      -- page          <- obj .:? "page"
      publication_date   <- obj .:? "publication_date"
      --publication_second <- obj .:? "publication_second"
      --publication_minute <- obj .:? "publication_minute"
      --publication_hour   <- obj .:? "publication_hour"
      publication_day    <- obj .:? "publication_day"
      publication_month  <- obj .:? "publication_month"
      publication_year   <- obj .:? "publication_year"
      source             <- obj .:? "sources"
      institutes         <- obj .:? "institutes"
      title              <- obj .:? "title"
      uniqId             <- obj .:? "uniqId"
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

docViewWrapper :: Record Props -> R.Element
docViewWrapper props = R.createElement docViewWrapperCpt props []

docViewWrapperCpt :: R.Component Props
docViewWrapperCpt = R.hooksComponent "G.C.N.C.D.docViewWrapper" cpt
  where
    cpt { loaded, path } _ = do
      state <- R.useState' $ initialState { loaded }

      pure $ docView { loaded, path, state }

type DocViewProps = (
  state :: R.State State
  | Props
  )

docView :: Record DocViewProps -> R.Element
docView props = R.createElement docViewCpt props []

docViewCpt :: R.Component DocViewProps
docViewCpt = R.hooksComponent "G.C.N.C.D.docView" cpt
  where
    cpt props@{ loaded: loaded@{ ngramsTable: Versioned { data: initTable }, document }, state } _ = do
      pure $ H.div {} [
        autoUpdate { duration: 3000, effect: dispatch Synchronize }
      , H.div { className: "container1" }
        [
          R2.row
          [
            R2.col 8
            [ H.h4 {} [ annotate state doc.title ]
            , H.ul { className: "list-group" }
              [ li' [ H.span {} [ text' doc.source ]
                    , badge "source"
                    ]
              -- TODO add href to /author/ if author present in
              , li' [ H.span {} [ text' doc.authors ]
                    , badge "authors"
                    ]
              , li' [ H.span {} [ H.text $ publicationDate $ Document doc ]
                    , badge "date"
                    ]
              ]
            , badge "abstract"
            , annotate state doc.abstract
            , H.div { className: "jumbotron" }
              [ H.p {} [ H.text "Empty Full Text" ]
              ]
            ]
          ]
        ]
      ]
        where
          dispatch :: Action -> Effect Unit
          dispatch (AddNewNgram ngram termList) = do
            commitPatchR (Versioned {version, data: pt}) state
            where
              ({ ngramsVersion: version } /\ _) = state
              pt = addNewNgram ngram termList
          dispatch (SetTermListItem ngram termList) = do
            commitPatchR (Versioned {version, data: pt}) state
            where
              ({ ngramsVersion: version } /\ _) = state
              pe = NgramsPatch { patch_list: termList, patch_children: mempty }
              pt = singletonNgramsTablePatch ngram pe
          dispatch Synchronize = do
            syncPatchesR props.path props.state

          annotate state text = AnnotatedField.annotatedField { ngrams: ngramsTable state
                                                              , setTermList: setTermList state
                                                              , text }
          badge s = H.span { className: "badge badge-default badge-pill" } [ H.text s ]
          li' = H.li { className: "list-group-item justify-content-between" }
          ngramsTable ({ ngramsLocalPatch, ngramsValidPatch } /\ _) = applyNgramsTablePatch (ngramsLocalPatch <> ngramsValidPatch) initTable
          setTermList state ngram Nothing        newList = dispatch (AddNewNgram ngram newList)
          setTermList state ngram (Just oldList) newList = dispatch (SetTermListItem ngram (replace oldList newList))
          text' x = H.text $ fromMaybe "Nothing" x
          NodePoly {hyperdata : Document doc} = document

type LayoutProps = (
    corpusId :: Maybe Int
  , listId :: Int
  , nodeId :: Int
  , session :: Session
  )

documentLayout :: Record LayoutProps -> R.Element
documentLayout props = R.createElement documentLayoutCpt props []

documentLayoutCpt :: R.Component LayoutProps
documentLayoutCpt = R.hooksComponent "G.C.N.C.D.documentLayout" cpt
  where
    cpt { corpusId, listId, nodeId, session } _ = do
      let sid = sessionId session

      pure $ documentLayoutWithKey { corpusId
                                   , key: show sid <> "-" <> show nodeId
                                   , listId
                                   , nodeId
                                   , session }

type KeyLayoutProps = (
  key :: String
  | LayoutProps
  )

documentLayoutWithKey :: Record KeyLayoutProps -> R.Element
documentLayoutWithKey props = R.createElement documentLayoutWithKeyCpt props []

documentLayoutWithKeyCpt :: R.Component KeyLayoutProps
documentLayoutWithKeyCpt = R.hooksComponent "G.C.N.C.D.documentLayoutWithKey" cpt
  where
    cpt { corpusId, listId, nodeId, session } _ = do
      useLoader path loadData $ \loaded ->
        docViewWrapper {path, loaded}
      where
        tabType = TabDocument (TabNgramType CTabTerms)
        path = { corpusId, listIds: [listId], nodeId, session, tabType }

------------------------------------------------------------------------

loadDocument :: Session -> Int -> Aff NodeDocument
loadDocument session nodeId = get session $ NodeAPI Node (Just nodeId) ""

loadData :: DocPath -> Aff LoadedData
loadData {session, nodeId, listIds, tabType} = do
  document <- loadDocument session nodeId
  ngramsTable <- loadNgramsTable
    { session
    , nodeId
    , listIds
    , params: { offset : 0, limit : 100, orderBy: Nothing, searchType: SearchDoc}
    , tabType
    , searchQuery: ""
    , termListFilter: Nothing
    , termSizeFilter: Nothing
    , scoreType: Occurrences
    }
  pure {document, ngramsTable}
