module Gargantext.Components.Nodes.Lists where

import Gargantext.Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Gargantext.Components.App.Store (Boxes)
import Gargantext.Core.NgramsTable.Types (NgramsTerm)
import Gargantext.Components.Corpus.CodeSection (loadCorpusWithChild)
import Gargantext.Components.NgramsTable.Loader (clearCache)
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.Nodes.Lists.SidePanel (SidePanel)
import Gargantext.Components.Nodes.Lists.Tabs as Tabs
import Gargantext.Components.Nodes.Lists.Types (CacheState(..))
import Gargantext.Components.Table as Table
import Gargantext.Config.REST (logRESTError)
import Gargantext.Hooks.Loader (useLoader)
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
  ( selectedNgrams :: T.Box (Maybe NgramsTerm)
  , session        :: Session
  , sidePanelState :: T.Box GT.SidePanelState
  )

sidePanel :: R2.Component SidePanelProps
sidePanel = R.createElement sidePanelCpt
sidePanelCpt :: R.Component SidePanelProps
sidePanelCpt = here.component "sidePanel" cpt
  where
    cpt { session
        , selectedNgrams
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
      , sidePanelNgramsContextView { selectedNgrams
                                   , session } []
      ]

type SidePanelNgramsContextView =
 ( selectedNgrams :: T.Box (Maybe NgramsTerm)
 , session        :: Session )

sidePanelNgramsContextView :: R2.Component SidePanelNgramsContextView
sidePanelNgramsContextView = R.createElement sidePanelNgramsContextViewCpt
sidePanelNgramsContextViewCpt :: R.Component SidePanelNgramsContextView
sidePanelNgramsContextViewCpt = here.component "sidePanelNgramsContextView" cpt where
  cpt { selectedNgrams
      , session } _ = do
    -- pure $ H.h4 {} [ H.text txt ]
    pure $ H.div {} [ H.text "Hello ngrams" ]
