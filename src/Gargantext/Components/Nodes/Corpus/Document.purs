module Gargantext.Components.Nodes.Corpus.Document where

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude (bind, pure, show, unit, ($), (<>))

import Gargantext.Components.AutoUpdate (autoUpdate)
import Gargantext.Components.Search (SearchType(..))
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.Nodes.Corpus.Document.Types (DocPath, Document(..), LoadedData, NodeDocument, Props, State, initialState)
import Gargantext.Components.NgramsTable.Core
  ( CoreAction(..), Versioned(..), addNewNgramA, applyNgramsPatches, coreDispatch, loadNgramsTable
  , replace, setTermListA, syncResetButtons )
import Gargantext.Components.Annotation.AnnotatedField as AnnotatedField
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, get, sessionId)
import Gargantext.Types (CTabNgramType(..), NodeType(..), TabSubType(..), TabType(..), ScoreType(..))
import Gargantext.Utils as U
import Gargantext.Utils.Reactix as R2

thisModule :: String
thisModule = "Gargantext.Components.Nodes.Corpus.Document"

publicationDate :: Document -> String
publicationDate (Document doc@{publication_year: Nothing}) = ""
publicationDate (Document doc@{publication_year: Just py, publication_month: Nothing}) = U.zeroPad 2 py
publicationDate (Document doc@{publication_year: Just py, publication_month: Just pm, publication_day: Nothing}) = (U.zeroPad 2 py) <> "-" <> (U.zeroPad 2 pm)
publicationDate (Document doc@{publication_year: Just py, publication_month: Just pm, publication_day: Just pd}) = (U.zeroPad 2 py) <> "-" <> (U.zeroPad 2 pm) <> "-" <> (U.zeroPad 2 pd)

docViewWrapper :: Record Props -> R.Element
docViewWrapper props = R.createElement docViewWrapperCpt props []

docViewWrapperCpt :: R.Component Props
docViewWrapperCpt = R.hooksComponentWithModule thisModule "docViewWrapper" cpt
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
docViewCpt = R.hooksComponentWithModule thisModule "docView" cpt
  where
    cpt { path
        , loaded: loaded@{ ngramsTable: Versioned { data: initTable }, document }
        , state: state@({ ngramsVersion: version, ngramsLocalPatch } /\ _)
        } _children = do

      let
        afterSync = \_ -> pure unit
        syncResetBtns = [syncResetButtons { afterSync, ngramsLocalPatch
                                          , performAction: dispatch
                                          }]
        withAutoUpdate = false
        autoUpd :: Array R.Element
        autoUpd = if withAutoUpdate then
                     [ autoUpdate { duration: 5000
                                  , effect: dispatch $ Synchronize { afterSync }
                                  }
                     ]
                  else []

      pure $ H.div {} $
        autoUpd <> syncResetBtns <> [
        H.div { className: "container1" }
        [
          R2.row
          [
            R2.col 8
            [ H.h4 {} [ annotate doc.title ]
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
            , annotate doc.abstract
            , H.div { className: "jumbotron" }
              [ H.p {} [ H.text "Empty Full Text" ]
              ]
            ]
          ]
        ]
      ]
        where
          dispatch = coreDispatch path state
          ngrams = applyNgramsPatches (fst state) initTable
          annotate text = AnnotatedField.annotatedField { ngrams
                                                        , setTermList
                                                        , text }
          badge s = H.span { className: "badge badge-default badge-pill" } [ H.text s ]
          li' = H.li { className: "list-group-item justify-content-between" }
          setTermList ngram Nothing        newList = dispatch (addNewNgramA ngram newList)
          setTermList ngram (Just oldList) newList = dispatch (setTermListA ngram (replace oldList newList))
          text' x = H.text $ fromMaybe "Nothing" x
          NodePoly {hyperdata: Document doc} = document

type LayoutProps = (
    corpusId :: Maybe Int
  , listId :: Int
  , nodeId :: Int
  , session :: Session
  )

documentLayout :: Record LayoutProps -> R.Element
documentLayout props = R.createElement documentLayoutCpt props []

documentLayoutCpt :: R.Component LayoutProps
documentLayoutCpt = R.hooksComponentWithModule thisModule "documentLayout" cpt
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
documentLayoutWithKeyCpt = R.hooksComponentWithModule thisModule "documentLayoutWithKey" cpt
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
