module Gargantext.Components.Nodes.Lists where

import DOM.Simple.Console (log, log2)
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Gargantext.AsyncTasks as GAT
import Gargantext.Components.Forest as Forest
import Gargantext.Components.NgramsTable.Loader (clearCache)
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.Nodes.Corpus (loadCorpusWithChild)
import Gargantext.Components.Nodes.Corpus.Types (getCorpusInfo, CorpusInfo(..), Hyperdata(..))
import Gargantext.Components.Nodes.Lists.Tabs as Tabs
import Gargantext.Components.Nodes.Lists.Types (CacheState(..), ListsLayoutControls, SidePanelState(..), initialControls, toggleSidePanelState)
import Gargantext.Components.Table as Table
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Prelude (Unit, bind, const, discard, pure, show, unit, ($), (<>))
import Gargantext.Sessions (WithSession, WithSessionContext, Session, sessionId, getCacheState, setCacheState)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Record.Extra as REX
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Lists"

listsWithSessionContext :: R2.Component CommonPropsSessionContext
listsWithSessionContext = R.createElement listsWithSessionContextCpt

listsWithSessionContextCpt :: R.Component CommonPropsSessionContext
listsWithSessionContextCpt = here.component "listsWithSessionContext" cpt where
  cpt props@{ session } _ = do
      session' <- R.useContext session
      controls <- initialControls

      pure $ R.fragment [
        -- topBar { controls } []
        listsLayout (Record.merge { controls, session: session' } props) []
      , H.div { className: "side-panel" } [ sidePanel { controls, session: session' } [] ]
      ]
--------------------------------------------------------

type CommonPropsNoSession =
  ( nodeId        :: Int
  , reloadForest  :: T.Box T2.Reload
  , reloadRoot    :: T.Box T2.Reload
  , sessionUpdate :: Session -> Effect Unit
  , tasks         :: T.Box GAT.Storage
  )

type CommonProps = WithSession CommonPropsNoSession

type CommonPropsSessionContext = WithSessionContext CommonPropsNoSession

type Props = ( controls :: Record ListsLayoutControls | CommonProps )

type WithTreeProps = ( handed :: GT.Handed | Props )

listsLayout :: R2.Component Props
listsLayout = R.createElement listsLayoutCpt

listsLayoutCpt :: R.Component Props
listsLayoutCpt = here.component "listsLayout" cpt where
  cpt props@{ nodeId, session } _ = do
    let sid = sessionId session
    pure $ listsLayoutWithKey $ Record.merge props { key: show sid <> "-" <> show nodeId }

type KeyProps = ( key :: String | Props )

listsLayoutWithKey :: Record KeyProps -> R.Element
listsLayoutWithKey props = R.createElement listsLayoutWithKeyCpt props []

listsLayoutWithKeyCpt :: R.Component KeyProps
listsLayoutWithKeyCpt = here.component "listsLayoutWithKey" cpt where
    cpt { controls, nodeId, reloadForest, reloadRoot, session, sessionUpdate, tasks } _ = do
      let path = { nodeId, session }

      cacheState <- T.useBox $ getCacheState CacheOn session nodeId
      cacheState' <- T.useLive T.unequal cacheState

      R.useEffectOnce' $ do
        T.listen (\{ new } -> afterCacheStateChange new) cacheState

      useLoader path loadCorpusWithChild $
        \corpusData@{ corpusId, corpusNode: NodePoly poly, defaultListId } ->
          let { date, hyperdata : Hyperdata h, name } = poly
              CorpusInfo { authors, desc, query } = getCorpusInfo h.fields
          in
          R.fragment [
            Table.tableHeaderLayout {
                cacheState
              , date
              , desc
              , key: "listsLayoutWithKey-header-" <> (show cacheState')
              , query
              , title: "Corpus " <> name
              , user: authors } []
          , Tabs.tabs {
               cacheState
             , corpusData
             , corpusId
             , key: "listsLayoutWithKey-tabs-" <> (show cacheState')
             , reloadForest
             , reloadRoot
             , session
             , sidePanelTriggers: controls.triggers
             , tasks
             }
          ]
      where
        afterCacheStateChange cacheState = do
          launchAff_ $ clearCache unit
          sessionUpdate $ setCacheState session nodeId cacheState

type SidePanelProps =
  ( controls :: Record ListsLayoutControls
  , session  :: Session
  )

sidePanel :: R2.Component SidePanelProps
sidePanel = R.createElement sidePanelCpt

sidePanelCpt :: R.Component SidePanelProps
sidePanelCpt = here.component "sidePanel" cpt
  where
    cpt { controls: { triggers: { toggleSidePanel
                                , triggerSidePanel
                                } }
        , session } _ = do

      showSidePanel <- R.useState' InitialClosed

      R.useEffect' $ do
        let toggleSidePanel' _  = snd showSidePanel toggleSidePanelState
            triggerSidePanel' _ = snd showSidePanel $ const Opened
        R2.setTrigger toggleSidePanel  toggleSidePanel'
        R2.setTrigger triggerSidePanel triggerSidePanel'

      (mCorpusId /\ setMCorpusId) <- R.useState' Nothing
      (mListId   /\ setMListId  ) <- R.useState' Nothing
      (mNodeId   /\ setMNodeId  ) <- R.useState' Nothing

      let mainStyle = case fst showSidePanel of
            Opened -> { display: "block" }
            _      -> { display: "none" }

      let closeSidePanel _ = do
            snd showSidePanel $ const Closed

      pure $ H.div { style: mainStyle } [
        H.div { className: "header" } [
          H.span { className: "btn btn-danger"
                 , on: { click: closeSidePanel } } [
            H.span { className: "fa fa-times" } []
          ]
        ]
      , sidePanelDocView { session } []
      ]

type SidePanelDocView = ( session :: Session )

sidePanelDocView :: R2.Component SidePanelDocView
sidePanelDocView = R.createElement sidePanelDocViewCpt

sidePanelDocViewCpt :: R.Component SidePanelDocView
sidePanelDocViewCpt = here.component "sidePanelDocView" cpt where
  cpt { session } _ = do
    -- pure $ H.h4 {} [ H.text txt ]
    pure $ H.div {} [ H.text "Hello ngrams" ]
