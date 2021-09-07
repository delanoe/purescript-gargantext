module Gargantext.Components.Nodes.Corpus.Document where

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Aff (Aff)
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Toestand as T

import Gargantext.Prelude (bind, pure, show, unit, ($), (<>), (<$>), (<<<))

import Gargantext.Components.AutoUpdate (autoUpdate)
import Gargantext.Components.Search (SearchType(..))
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.Nodes.Corpus.Document.Types (DocPath, Document(..), LoadedData, NodeDocument, Props, State, initialState)
import Gargantext.Components.NgramsTable.Core
  ( CoreAction(..), Versioned(..), addNewNgramA, applyNgramsPatches, coreDispatch, loadNgramsTable
  , replace, setTermListA, syncResetButtons, findNgramRoot )
import Gargantext.Components.Annotation.AnnotatedField as AnnotatedField
import Gargantext.Config.REST (RESTError)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, get, sessionId)
import Gargantext.Types (CTabNgramType(..), ListId, NodeID, NodeType(..), TabSubType(..), TabType(..), ScoreType(..))
import Gargantext.Utils as U
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Corpus.Document"

publicationDate :: Document -> String
publicationDate (Document {publication_year: Nothing}) = ""
publicationDate (Document {publication_year: Just py, publication_month: Nothing}) = U.zeroPad 2 py
publicationDate (Document {publication_year: Just py, publication_month: Just pm, publication_day: Nothing}) = (U.zeroPad 2 py) <> "-" <> (U.zeroPad 2 pm)
publicationDate (Document {publication_year: Just py, publication_month: Just pm, publication_day: Just pd}) = (U.zeroPad 2 py) <> "-" <> (U.zeroPad 2 pm) <> "-" <> (U.zeroPad 2 pd)

docViewWrapper :: R2.Component Props
docViewWrapper = R.createElement docViewWrapperCpt
docViewWrapperCpt :: R.Component Props
docViewWrapperCpt = here.component "docViewWrapper" cpt
  where
    cpt props@{ loaded } _ = do
      state <- T.useBox $ initialState { loaded }

      pure $ docView (Record.merge props { state }) []

type DocViewProps = (
  state :: T.Box State
  | Props
  )

docView :: R2.Component DocViewProps
docView = R.createElement docViewCpt
docViewCpt :: R.Component DocViewProps
docViewCpt = here.component "docView" cpt
  where
    cpt { path
        , loaded: { ngramsTable: Versioned { data: initTable }, document }
        , state
        } _children = do
      state'@{ ngramsLocalPatch } <- T.useLive T.unequal state

      let
        afterSync = \_ -> pure unit
        syncResetBtns =
          [ syncResetButtons { afterSync, ngramsLocalPatch, performAction: dispatch } ]
        withAutoUpdate = false
        autoUpd :: Array R.Element
        autoUpd =
          if withAutoUpdate
          then [ autoUpdate { duration: 5000, effect: dispatch $ Synchronize { afterSync } } ]
          else []

        ngrams = applyNgramsPatches state' initTable
        annotate text = AnnotatedField.annotatedField { ngrams, setTermList, text }

        setTermListOrAddA ngram Nothing        = addNewNgramA ngram
        setTermListOrAddA ngram (Just oldList) = setTermListA ngram <<< replace oldList
        setTermList ngram mOldList = dispatch <<< setTermListOrAddA (findNgramRoot ngrams ngram) mOldList

      pure $ H.div {} $
        autoUpd <> syncResetBtns <>
        --DEBUG
        --[ H.pre { rows: 30 } [
        --    H.text (stringifyWithIndent 2 (encodeJson (fst state)))
        --  ] ] <>
        [ H.div { className: "corpus-doc-view container1" }
          [ R2.row
            [ R2.col 12
              [ H.h4 {} [ H.span {} [ badge "title", annotate doc.title [] ] ]
              , H.ul { className: "list-group" }
                [ li' [ badgeLi "source", text' doc.source ]
              -- TODO add href to /author/ if author present in
                , li' [ badgeLi "authors", text' doc.authors ]
                , li' [ badgeLi "date", H.text $ publicationDate $ Document doc ]
                ]
              , H.span {} [ badge "abstract", annotate doc.abstract [] ]
              , H.div { className: "jumbotron" } [ H.p {} [ H.text "Empty Full Text" ] ]
              ]]]]
      where
        dispatch = coreDispatch path state
        badge s = H.span { className: "badge badge-default badge-pill" } [ H.text s ]
        badgeLi s =
          H.span { className: "list-group-item-heading" }
          [ H.span { className: "badge-container" }
            [ H.span { className: "badge badge-default badge-pill" } [ H.text s ] ]]
        li' = H.li { className: "list-group-item justify-content-between" }
        -- Here the use of findNgramRoot makes that we always target the root of an ngram group.
        text' x = H.span { className: "list-group-item-text" } [ H.text $ fromMaybe "Nothing" x ]
        NodePoly {hyperdata: Document doc} = document

type LayoutProps =
 (  listId    :: ListId
  , mCorpusId :: Maybe NodeID
  , nodeId    :: NodeID
  , session   :: Session
  )

documentMainLayout :: R2.Component LayoutProps
documentMainLayout = R.createElement documentMainLayoutCpt
documentMainLayoutCpt :: R.Component LayoutProps
documentMainLayoutCpt = here.component "documentMainLayout" cpt where
    cpt props _ = pure $ R2.row [ R2.col 10 [ documentLayout props [] ] ]

documentLayout :: R2.Component LayoutProps
documentLayout = R.createElement documentLayoutCpt
documentLayoutCpt :: R.Component LayoutProps
documentLayoutCpt = here.component "documentLayout" cpt where
  cpt { listId, mCorpusId, nodeId, session } children = do
    pure $ documentLayoutWithKey { key, listId, mCorpusId, nodeId, session } children
      where
        key = show (sessionId session) <> "-" <> show nodeId

type KeyLayoutProps =
  ( key      :: String
  , listId   :: ListId
  , mCorpusId :: Maybe NodeID
  , nodeId   :: NodeID
  , session  :: Session
  )

documentLayoutWithKey :: R2.Component KeyLayoutProps
documentLayoutWithKey = R.createElement documentLayoutWithKeyCpt
documentLayoutWithKeyCpt :: R.Component KeyLayoutProps
documentLayoutWithKeyCpt = here.component "documentLayoutWithKey" cpt
  where
    cpt { listId, mCorpusId, nodeId, session } _ = do
      useLoader { errorHandler
                , loader: loadData
                , path
                , render: \loaded -> docViewWrapper { loaded, path } [] }
      where
        tabType = TabDocument (TabNgramType CTabTerms)
        path = { listIds: [listId], mCorpusId, nodeId, session, tabType }
        errorHandler err = here.log2 "[documentLayoutWithKey] RESTError" err

------------------------------------------------------------------------

loadDocument :: Session -> Int -> Aff (Either RESTError NodeDocument)
loadDocument session nodeId = get session $ NodeAPI Node (Just nodeId) ""

loadData :: DocPath -> Aff (Either RESTError LoadedData)
loadData { listIds, nodeId, session, tabType } = do
  eDocument <- loadDocument session nodeId
  case eDocument of
    Left err -> pure $ Left err
    Right document -> do
      eNgramsTable <- loadNgramsTable
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
      pure $ (\ngramsTable -> { document, ngramsTable }) <$> eNgramsTable
