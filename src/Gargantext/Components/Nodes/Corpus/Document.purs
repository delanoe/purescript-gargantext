module Gargantext.Components.Nodes.Corpus.Document where

--import Data.Argonaut (encodeJson) -- DEBUG
--import Data.Argonaut.Core (stringifyWithIndent) -- DEBUG
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record

import Gargantext.Prelude (bind, pure, show, unit, ($), (<>), (<<<))

import Gargantext.Components.AutoUpdate (autoUpdate)
import Gargantext.Components.Search (SearchType(..))
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.Nodes.Corpus.Document.Types (DocPath, Document(..), LoadedData, NodeDocument, Props, State, initialState)
import Gargantext.Components.NgramsTable.Core
  ( CoreAction(..), Versioned(..), addNewNgramA, applyNgramsPatches, coreDispatch, loadNgramsTable
  , replace, setTermListA, syncResetButtons, findNgramRoot )
import Gargantext.Components.Annotation.AnnotatedField as AnnotatedField
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, get, sessionId)
import Gargantext.Types (CTabNgramType(..), ListId, NodeID, NodeType(..), TabSubType(..), TabType(..), ScoreType(..))
import Gargantext.Utils as U
import Gargantext.Utils.Reactix as R2

thisModule :: String
thisModule = "Gargantext.Components.Nodes.Corpus.Document"

publicationDate :: Document -> String
publicationDate (Document doc@{publication_year: Nothing}) = ""
publicationDate (Document doc@{publication_year: Just py, publication_month: Nothing}) = U.zeroPad 2 py
publicationDate (Document doc@{publication_year: Just py, publication_month: Just pm, publication_day: Nothing}) = (U.zeroPad 2 py) <> "-" <> (U.zeroPad 2 pm)
publicationDate (Document doc@{publication_year: Just py, publication_month: Just pm, publication_day: Just pd}) = (U.zeroPad 2 py) <> "-" <> (U.zeroPad 2 pm) <> "-" <> (U.zeroPad 2 pd)

docViewWrapper :: R2.Component Props
docViewWrapper = R.createElement docViewWrapperCpt

docViewWrapperCpt :: R.Component Props
docViewWrapperCpt = R.hooksComponentWithModule thisModule "docViewWrapper" cpt
  where
    cpt props@{ loaded } _ = do
      state <- R.useState' $ initialState { loaded }

      pure $ docView (Record.merge props { state }) []

type DocViewProps = (
  state :: R.State State
  | Props
  )

docView :: R2.Component DocViewProps
docView = R.createElement docViewCpt

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
        autoUpd <> syncResetBtns <>
        --DEBUG
        --[ H.pre { rows: 30 } [
        --    H.text (stringifyWithIndent 2 (encodeJson (fst state)))
        --  ] ] <>
        [
        H.div { className: "container1" }
        [
          R2.row
          [
            R2.col 12
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
          setTermListOrAddA ngram Nothing        = addNewNgramA ngram
          setTermListOrAddA ngram (Just oldList) = setTermListA ngram <<< replace oldList
          setTermList ngram mOldList = dispatch <<< setTermListOrAddA (findNgramRoot ngrams ngram) mOldList
          -- Here the use of findNgramRoot makes that we always target the root of an ngram group.
          text' x = H.text $ fromMaybe "Nothing" x
          NodePoly {hyperdata: Document doc} = document

type LayoutProps = (
    listId         :: ListId
  , mCorpusId      :: Maybe NodeID
  , nodeId         :: NodeID
  , session        :: Session
  )

documentMainLayout :: R2.Component LayoutProps
documentMainLayout = R.createElement documentMainLayoutCpt

documentMainLayoutCpt :: R.Component LayoutProps
documentMainLayoutCpt = R.hooksComponentWithModule thisModule "documentMainLayout" cpt
  where
    cpt props _ = do
      pure $ R2.row [
        R2.col 10 [
           documentLayout props []
           ]
        ]

documentLayout :: R2.Component LayoutProps
documentLayout = R.createElement documentLayoutCpt

documentLayoutCpt :: R.Component LayoutProps
documentLayoutCpt = R.hooksComponentWithModule thisModule "documentLayout" cpt
  where
    cpt props@{ nodeId, session } _ = do
      let sid = sessionId session

      pure $ documentLayoutWithKey (Record.merge props { key: show sid <> "-" <> show nodeId }) []

type KeyLayoutProps = (
  key :: String
  | LayoutProps
  )

documentLayoutWithKey :: R2.Component KeyLayoutProps
documentLayoutWithKey = R.createElement documentLayoutWithKeyCpt

documentLayoutWithKeyCpt :: R.Component KeyLayoutProps
documentLayoutWithKeyCpt = R.hooksComponentWithModule thisModule "documentLayoutWithKey" cpt
  where
    cpt { listId, mCorpusId, nodeId, session } _ = do
      useLoader path loadData $ \loaded ->
        docViewWrapper { loaded, path } []
      where
        tabType = TabDocument (TabNgramType CTabTerms)
        path = { listIds: [listId], mCorpusId, nodeId, session, tabType }

------------------------------------------------------------------------

loadDocument :: Session -> Int -> Aff NodeDocument
loadDocument session nodeId = get session $ NodeAPI Node (Just nodeId) ""

loadData :: DocPath -> Aff LoadedData
loadData { listIds, nodeId, session, tabType } = do
  document <- loadDocument session nodeId
  ngramsTable <- loadNgramsTable
    { listIds
    , nodeId
    , params: { offset : 0, limit : 100, orderBy: Nothing, searchType: SearchDoc}
    , scoreType: Occurrences
    , searchQuery: ""
    , session
    , tabType
    , termListFilter: Nothing
    , termSizeFilter: Nothing
    }
  pure { document, ngramsTable }
