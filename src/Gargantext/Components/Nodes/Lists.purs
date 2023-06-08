module Gargantext.Components.Nodes.Lists where

import Gargantext.Prelude

import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Gargantext.Components.App.Store (Boxes)
import Gargantext.Components.Corpus.CodeSection (loadCorpusWithChild)
import Gargantext.Components.GraphQL.Context as GQLCTX
import Gargantext.Components.GraphQL.Endpoints (getContextsForNgrams)
import Gargantext.Components.NgramsTable.Loader (clearCache)
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.Nodes.Lists.SidePanel (SidePanel)
import Gargantext.Components.Nodes.Lists.Tabs as Tabs
import Gargantext.Components.Nodes.Lists.Types (CacheState(..))
import Gargantext.Components.Table as Table
import Gargantext.Config (defaultFrontends)
import Gargantext.Config.REST (logRESTError, AffRESTError)
import Gargantext.Core.NgramsTable.Types (NgramsTerm(..))
import Gargantext.Ends (url)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes as Routes
import Gargantext.Sessions (WithSession, WithSessionContext, Session, sessionId, getCacheState, setCacheState)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Lists"
--------------------------------------------------------

type CommonPropsNoSession =
  ( boxes         :: Boxes
  , nodeId        :: Int
  , sessionUpdate :: Session -> Effect Unit
  , sidePanel     :: T.Box (Maybe (Record SidePanel))
  )

type Props = WithSession CommonPropsNoSession

type CommonPropsSessionContext = WithSessionContext CommonPropsNoSession

type WithTreeProps = ( handed :: GT.Handed | Props )

listsLayout :: R2.Component Props
listsLayout = R.createElement listsLayoutCpt
listsLayoutCpt :: R.Component Props
listsLayoutCpt = here.component "listsLayout" cpt where
  cpt props@{ nodeId, session } _ = do
    let sid = sessionId session
    pure $ listsLayoutWithKey (Record.merge props { key: show sid <> "-" <> show nodeId }) []

listsLayoutWithKey :: R2.Component ( key :: String | Props )
listsLayoutWithKey = R.createElement listsLayoutWithKeyCpt
listsLayoutWithKeyCpt :: R.Component ( key :: String | Props )
listsLayoutWithKeyCpt = here.component "listsLayoutWithKey" cpt where
  cpt { boxes
      , nodeId
      , session
      , sessionUpdate
      , sidePanel } _ = do
    activeTab <- T.useBox 0

    let path = { nodeId, session }

    cacheState <- T.useBox $ getCacheState CacheOff session nodeId
    cacheState' <- T.useLive T.unequal cacheState

    R.useEffectOnce' $ do
      T.listen (\{ new } -> afterCacheStateChange new) cacheState

    useLoader { errorHandler
              , path
              , loader: loadCorpusWithChild
              , render: \corpusData@{ corpusId, corpusNode: NodePoly poly } ->
                          let { name, date, hyperdata } = poly
                          in
                            R.fragment [
                              Table.tableHeaderWithRenameLayout {
                                cacheState
                              , name
                              , date
                              , hyperdata
                              , nodeId: corpusId
                              , session
                              , key: "listsLayoutWithKey-header-" <> (show cacheState')
                                }
                            , Tabs.tabs {
                                activeTab
                              , boxes
                              , cacheState
                              , corpusData
                              , corpusId
                              , key: "listsLayoutWithKey-tabs-" <> (show cacheState')
                              , session
                              , sidePanel
                              }
                            ] }
    where
      errorHandler = logRESTError here "[listsLayoutWithKey]"
      afterCacheStateChange cacheState = do
        launchAff_ $ clearCache unit
        sessionUpdate $ setCacheState session nodeId cacheState

type SidePanelProps =
  ( session        :: Session
  , sidePanel      :: T.Box (Maybe (Record SidePanel))
  , sidePanelState :: T.Box GT.SidePanelState
  )

sidePanel :: R2.Component SidePanelProps
sidePanel = R.createElement sidePanelCpt
sidePanelCpt :: R.Component SidePanelProps
sidePanelCpt = here.component "sidePanel" cpt
  where
    cpt { session
        , sidePanel
        , sidePanelState } _ = do

      sidePanelState' <- T.useLive T.unequal sidePanelState

      let mainStyle = case sidePanelState' of
            GT.Opened -> { display: "block" }
            _         -> { display: "none" }

      let closeSidePanel _ = T.write_ GT.Closed sidePanelState

      pure $ H.div { style: mainStyle } [
        H.div { className: "header" } [
          H.span { className: "btn btn-danger"
                 , on: { click: closeSidePanel } } [
            H.span { className: "fa fa-times" } []
          ]
        ]
      , sidePanelNgramsContextView { session
                                   , sidePanel } []
      ]

type SidePanelNgramsContextView =
 ( session        :: Session
 , sidePanel      :: T.Box (Maybe (Record SidePanel)) )

sidePanelNgramsContextView :: R2.Component SidePanelNgramsContextView
sidePanelNgramsContextView = R.createElement sidePanelNgramsContextViewCpt
sidePanelNgramsContextViewCpt :: R.Component SidePanelNgramsContextView
sidePanelNgramsContextViewCpt = here.component "sidePanelNgramsContextView" cpt where
  cpt { session
      , sidePanel } _ = do
    mSidePanel' <- T.useLive T.unequal sidePanel

    case mSidePanel' of
      Nothing -> pure $ H.div {} []
      Just sidePanel' -> do
        let ngrams = maybe "" (\(NormNgramsTerm n) -> n) sidePanel'.mCurrentNgrams

        pure $ H.div {} [ H.h3 {} [ H.text ngrams ]
                        , ngramsDocList { mCorpusId: sidePanel'.mCorpusId
                                        , mListId: sidePanel'.mListId
                                        , mNgrams: sidePanel'.mCurrentNgrams
                                        , session } [] ]

type NgramsDocListProps =
  ( mCorpusId :: Maybe GT.CorpusId
  , mListId   :: Maybe GT.ListId
  , mNgrams   :: Maybe NgramsTerm
  , session   :: Session )

ngramsDocList :: R2.Component NgramsDocListProps
ngramsDocList = R.createElement ngramsDocListCpt
ngramsDocListCpt :: R.Component NgramsDocListProps
ngramsDocListCpt = here.component "ngramsDocList" cpt where
  cpt { mCorpusId: Nothing } _ = do
    pure $ H.div {} []
  cpt { mListId: Nothing } _ = do
    pure $ H.div {} []
  cpt { mNgrams: Nothing } _ = do
    pure $ H.div {} []
  cpt { mCorpusId: Just corpusId
      , mListId: Just listId
      , mNgrams: Just ngrams
      , session } _ = do
    useLoader { errorHandler
              , path: { corpusId, ngrams, session }
              , loader: loaderNgramsDocList
              , render: \ctx -> ngramsDocListLoaded { contexts: ctx
                                                    , corpusId
                                                    , listId
                                                    , ngrams
                                                    , session } []
              }
    where
      errorHandler = logRESTError here "[ngramsDocList]"

type NgramsDocLoadProps =
  ( corpusId :: GT.CorpusId
  , ngrams   :: NgramsTerm
  , session  :: Session )

loaderNgramsDocList :: Record NgramsDocLoadProps -> AffRESTError (Array GQLCTX.Context)
loaderNgramsDocList { corpusId, ngrams: NormNgramsTerm ngrams, session } =
  getContextsForNgrams session corpusId [ngrams]

type NgramsDocListLoadedProps =
  ( contexts :: Array GQLCTX.Context
  , corpusId :: GT.CorpusId
  , listId   :: GT.ListId
  , ngrams   :: NgramsTerm
  , session  :: Session )

ngramsDocListLoaded :: R2.Component NgramsDocListLoadedProps
ngramsDocListLoaded = R.createElement ngramsDocListLoadedCpt
ngramsDocListLoadedCpt :: R.Component NgramsDocListLoadedProps
ngramsDocListLoadedCpt = here.component "ngramsDocListLoaded" cpt where
  cpt { contexts
      , corpusId
      , listId
      , ngrams
      , session } _ = do
    pure $ H.div { className: "ngrams-doc-list" }
      [ H.ul { className: "list-group" } ((\item -> contextItem { corpusId
                                                                , item
                                                                , listId
                                                                , session } [] ) <$> contexts)
      ]

type ContextItemProps =
  ( corpusId :: GT.CorpusId
  , item     :: GQLCTX.Context
  , listId   :: GT.ListId
  , session  :: Session )

contextItem :: R2.Component ContextItemProps
contextItem = R.createElement contextItemCpt
contextItemCpt :: R.Component ContextItemProps
contextItemCpt = here.component "contextItem" cpt where
  cpt { corpusId
      , item
      , listId
      , session } _ = do

    let route = Routes.CorpusDocument (sessionId session) corpusId listId item.c_id
        href = url defaultFrontends route

    pure $ H.a { className: "list-group-item text-decoration-none"
               , href
               , target: "_blank" }
      [ H.div { className: "context-item-title" }
          [ H.text $ maybe "" (_.hrd_title) item.c_hyperdata ]
      , H.div { className: "context-item-source"}
          [ H.text $ maybe "" (_.hrd_source) item.c_hyperdata ]
      , H.div { className: "context-item-date"}
          [ H.text $ (maybe "" (\h -> show h.hrd_publication_year) item.c_hyperdata) <>
                     "-" <>
                     (maybe "" (\h -> show h.hrd_publication_month) item.c_hyperdata) ] ]
