module Gargantext.Components.Nodes.Lists where

import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
------------------------------------------------------------------------
import Gargantext.AsyncTasks as GAT
import Gargantext.Components.Forest as Forest
import Gargantext.Components.NgramsTable.Loader (clearCache)
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.Nodes.Corpus (loadCorpusWithChild)
import Gargantext.Components.Nodes.Corpus.Types (getCorpusInfo, CorpusInfo(..), Hyperdata(..))
import Gargantext.Components.Nodes.Lists.Tabs as Tabs
import Gargantext.Components.Nodes.Lists.Types
import Gargantext.Components.Table as Table
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Prelude
import Gargantext.Sessions (Session, sessionId, getCacheState, setCacheState)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Reload as GUR

thisModule :: String
thisModule = "Gargantext.Components.Nodes.Lists"
------------------------------------------------------------------------
type ListsWithForest = (
    forestProps :: Record Forest.ForestLayoutProps
  , listsProps  :: Record CommonProps
  )

listsWithForest :: R2.Component ListsWithForest
listsWithForest = R.createElement listsWithForestCpt

listsWithForestCpt :: R.Component ListsWithForest
listsWithForestCpt = R.hooksComponentWithModule thisModule "listsWithForest" cpt
  where
    cpt { forestProps
        , listsProps: listsProps@{ session } } _ = do
      controls <- initialControls

      pure $ Forest.forestLayoutWithTopBar forestProps [
        topBar { controls } []
      , listsLayout (Record.merge listsProps { controls }) []
      , H.div { className: "side-panel" } [
          sidePanel { controls, session } []
        ]
      ]
--------------------------------------------------------

type TopBarProps = (
  controls :: Record ListsLayoutControls
  )

topBar :: R2.Component TopBarProps
topBar = R.createElement topBarCpt

topBarCpt :: R.Component TopBarProps
topBarCpt = R.hooksComponentWithModule thisModule "topBar" cpt
  where
    cpt { controls } _ = do
      -- empty for now because the button is moved to the side panel
      pure $ H.div {} []
        -- H.ul { className: "nav navbar-nav" } [
        --   H.li {} [
        --      sidePanelToggleButton { state: controls.showSidePanel } []
        --      ]
        --   ]  -- head (goes to top bar)
--------------------------------------------------------

type CommonProps = (
    reloadRoot     :: GUR.ReloadS
  , tasks :: R.Ref (Maybe GAT.Reductor)
  , nodeId        :: Int
  , session       :: Session
  , sessionUpdate :: Session -> Effect Unit
  , reloadForest :: GUR.ReloadWithInitializeRef
  )

type Props = (
  controls :: Record ListsLayoutControls
  | CommonProps
  )

type WithTreeProps = (
    handed :: GT.Handed
  | Props
  )

listsLayout :: R2.Component Props
listsLayout = R.createElement listsLayoutCpt

listsLayoutCpt :: R.Component Props
listsLayoutCpt = R.hooksComponentWithModule thisModule "listsLayout" cpt
  where
    cpt path@{ nodeId, session } _ = do
      let sid = sessionId session

      pure $ listsLayoutWithKey $ Record.merge path { key: show sid <> "-" <> show nodeId }

type KeyProps = (
  key :: String
  | Props
  )

listsLayoutWithKey :: Record KeyProps -> R.Element
listsLayoutWithKey props = R.createElement listsLayoutWithKeyCpt props []

listsLayoutWithKeyCpt :: R.Component KeyProps
listsLayoutWithKeyCpt = R.hooksComponentWithModule thisModule "listsLayoutWithKey" cpt
  where
    cpt { reloadRoot
        , tasks
        , controls
        , nodeId
        , session
        , sessionUpdate
        , reloadForest } _ = do
      let path = { nodeId, session }

      cacheState <- R.useState' $ getCacheState CacheOn session nodeId

      useLoader path loadCorpusWithChild $
        \corpusData@{ corpusId, corpusNode: NodePoly poly, defaultListId } ->
          let { date, hyperdata : Hyperdata h, name } = poly
              CorpusInfo { authors, desc, query } = getCorpusInfo h.fields
          in
          R.fragment [
            Table.tableHeaderLayout {
                afterCacheStateChange
              , cacheState
              , date
              , desc
              , key: "listsLayoutWithKey-header-" <> (show $ fst cacheState)
              , query
              , title: "Corpus " <> name
              , user: authors }
          , Tabs.tabs {
               reloadRoot
             , tasks
             , cacheState
             , corpusData
             , corpusId
             , key: "listsLayoutWithKey-tabs-" <> (show $ fst cacheState)
             , session
             , sidePanelTriggers: controls.triggers
             , reloadForest
             }
          ]
      where
        afterCacheStateChange cacheState = do
          launchAff_ $ clearCache unit
          sessionUpdate $ setCacheState session nodeId cacheState
------------------------------------------------------------------------

type SidePanelProps = (
    controls :: Record ListsLayoutControls
  , session  :: Session
  )

sidePanel :: R2.Component SidePanelProps
sidePanel = R.createElement sidePanelCpt

sidePanelCpt :: R.Component SidePanelProps
sidePanelCpt = R.hooksComponentWithModule thisModule "sidePanel" cpt
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
      (mListId /\ setMListId) <- R.useState' Nothing
      (mNodeId /\ setMNodeId) <- R.useState' Nothing

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

type SidePanelDocView = (
    session   :: Session
  )

sidePanelDocView :: R2.Component SidePanelDocView
sidePanelDocView = R.createElement sidePanelDocViewCpt

sidePanelDocViewCpt :: R.Component SidePanelDocView
sidePanelDocViewCpt = R.hooksComponentWithModule thisModule "sidePanelDocView" cpt
  where
    cpt { session } _ = do
      -- pure $ H.h4 {} [ H.text txt ]
      pure $ H.div {} [ H.text "Hello ngrams" ]
