module Gargantext.Components.Nodes.Lists where

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Gargantext.AsyncTasks as GAT
import Gargantext.Components.NgramsTable.Loader (clearCache)
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.Nodes.Corpus (loadCorpusWithChild)
import Gargantext.Components.Nodes.Corpus.Types (getCorpusInfo, CorpusInfo(..), Hyperdata(..))
import Gargantext.Components.Nodes.Lists.Tabs as Tabs
import Gargantext.Components.Nodes.Lists.Types (CacheState(..), SidePanel)
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
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Lists"
--------------------------------------------------------

type CommonPropsNoSession =
  ( nodeId         :: Int
  , reloadForest   :: T2.ReloadS
  , reloadMainPage :: T2.ReloadS
  , reloadRoot     :: T2.ReloadS
  , sessionUpdate  :: Session -> Effect Unit
  , sidePanel      :: T.Box (Maybe (Record SidePanel))
  , sidePanelState :: T.Box GT.SidePanelState
  , tasks          :: T.Box GAT.Storage
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

type KeyProps = ( key :: String | Props )

listsLayoutWithKey :: R2.Component KeyProps
listsLayoutWithKey = R.createElement listsLayoutWithKeyCpt

listsLayoutWithKeyCpt :: R.Component KeyProps
listsLayoutWithKeyCpt = here.component "listsLayoutWithKey" cpt where
  cpt { nodeId
      , reloadForest
      , reloadMainPage
      , reloadRoot
      , session
      , sessionUpdate
      , sidePanel
      , sidePanelState
      , tasks } _ = do
    activeTab <- T.useBox 0
    reloadMainPage' <- T.useLive T.unequal reloadMainPage

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
              activeTab
            , cacheState
            , corpusData
            , corpusId
            , key: "listsLayoutWithKey-tabs-" <> (show cacheState')
            , reloadForest
            , reloadRoot
            , session
            , tasks
            }
        ]
    where
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
