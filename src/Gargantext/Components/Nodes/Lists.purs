module Gargantext.Components.Nodes.Lists where

import Gargantext.Prelude (Unit, bind, const, discard, pure, show, unit, ($), (<>))
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Record.Extra as REX
import Toestand as T

import Gargantext.AsyncTasks as GAT
import Gargantext.Components.Forest as Forest
import Gargantext.Components.NgramsTable.Loader (clearCache)
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.Nodes.Corpus (loadCorpusWithChild)
import Gargantext.Components.Nodes.Corpus.Types
  ( getCorpusInfo, CorpusInfo(..), Hyperdata(..) )
import Gargantext.Components.Nodes.Lists.Tabs as Tabs
import Gargantext.Components.Nodes.Lists.Types
  ( CacheState(..), ListsLayoutControls, SidePanelState(..)
  , initialControls, toggleSidePanelState )

import Gargantext.Components.Table as Table
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Sessions (WithSession, WithSessionContext, Session, sessionId, getCacheState, setCacheState)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Lists"

type ListsWithForest =
  ( forestProps :: Record Forest.Props
  , listsProps  :: Record CommonProps
  )

type ListsWithForestSessionContext =
  ( forestProps :: Record Forest.Props
  , listsProps  :: Record CommonPropsSessionContext )

listsWithForestSessionContext :: R2.Component ListsWithForestSessionContext
listsWithForestSessionContext = R.createElement listsWithForestSessionContextCpt

listsWithForestSessionContextCpt :: R.Component ListsWithForestSessionContext
listsWithForestSessionContextCpt = here.component "listsWithForestSessionContext" cpt where
  cpt { forestProps, listsProps: listsProps@{ session } } _ = do
      session' <- R.useContext session

      pure $ listsWithForest
        { forestProps
        , listsProps: Record.merge { session: session' } $ (REX.pick listsProps :: Record CommonPropsNoSession)
        } []

listsWithForest :: R2.Component ListsWithForest
listsWithForest = R.createElement listsWithForestCpt

listsWithForestCpt :: R.Component ListsWithForest
listsWithForestCpt = here.component "listsWithForest" cpt where
  cpt { forestProps, listsProps: listsProps@{ session } } _ = do
    controls <- initialControls
    pure $ Forest.forestLayoutWithTopBar forestProps
      [ topBar { controls } []
      , listsLayout (Record.merge listsProps { controls }) []
      , H.div { className: "side-panel" } [ sidePanel { controls, session } [] ]
      ]

type TopBarProps = ( controls :: Record ListsLayoutControls )

topBar :: R2.Component TopBarProps
topBar = R.createElement topBarCpt

topBarCpt :: R.Component TopBarProps
topBarCpt = here.component "topBar" cpt where
  cpt { controls } _ = do
    -- empty for now because the button is moved to the side panel
    pure $ H.div {} []
      -- H.ul { className: "nav navbar-nav" } [
      --   H.li {} [
      --      sidePanelToggleButton { state: controls.showSidePanel } []
      --      ]
      --   ]  -- head (goes to top bar)

type CommonPropsNoSession =
  ( nodeId        :: Int
  , reloadForest  :: T.Box T2.Reload
  , reloadRoot    :: T.Box T2.Reload
  , sessionUpdate :: Session -> Effect Unit
  , tasks         :: T.Box (Maybe GAT.Reductor)
  )

type CommonProps = WithSession CommonPropsNoSession

type CommonPropsSessionContext = WithSessionContext CommonPropsNoSession

type Props = ( controls :: Record ListsLayoutControls | CommonProps )

type WithTreeProps = ( handed :: GT.Handed | Props )

listsLayout :: R2.Component Props
listsLayout = R.createElement listsLayoutCpt

listsLayoutCpt :: R.Component Props
listsLayoutCpt = here.component "listsLayout" cpt where
  cpt path@{ nodeId, session } _ = do
    let sid = sessionId session
    pure $ listsLayoutWithKey $ Record.merge path { key: show sid <> "-" <> show nodeId }

type KeyProps = ( key :: String | Props )

listsLayoutWithKey :: Record KeyProps -> R.Element
listsLayoutWithKey props = R.createElement listsLayoutWithKeyCpt props []

listsLayoutWithKeyCpt :: R.Component KeyProps
listsLayoutWithKeyCpt = here.component "listsLayoutWithKey" cpt where
    cpt { controls, nodeId, reloadForest, reloadRoot, session, sessionUpdate, tasks } _ = do
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
               cacheState
             , corpusData
             , corpusId
             , key: "listsLayoutWithKey-tabs-" <> (show $ fst cacheState)
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
sidePanelCpt = here.component "sidePanel" cpt where
  cpt { controls: { triggers: { toggleSidePanel, triggerSidePanel } }
      , session } _ = do
    showSidePanel <- R.useState' InitialClosed
    R.useEffect' $ do
      let toggleSidePanel'  _ = snd showSidePanel toggleSidePanelState
          triggerSidePanel' _ = snd showSidePanel $ const Opened
      R2.setTrigger toggleSidePanel  toggleSidePanel'
      R2.setTrigger triggerSidePanel triggerSidePanel'
    (mCorpusId /\ setMCorpusId) <- R.useState' Nothing
    (mListId /\ setMListId) <- R.useState' Nothing
    (mNodeId /\ setMNodeId) <- R.useState' Nothing
    let mainStyle = case fst showSidePanel of
         Opened -> { display: "block" }
         _      -> { display: "none" }
    let closeSidePanel _ =  snd showSidePanel $ const Closed
    pure $ H.div { style: mainStyle }
      [ H.div { className: "header" }
        [ H.span { className: "btn btn-danger", on: { click: closeSidePanel } }
          [ H.span { className: "fa fa-times" } [] ]]
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
