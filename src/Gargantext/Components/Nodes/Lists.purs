module Gargantext.Components.Nodes.Lists where

import Gargantext.Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Gargantext.Components.App.Store (Boxes)
import Gargantext.Components.NgramsTable.Loader (clearCache)
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.Nodes.Corpus (loadCorpusWithChild)
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
      , sessionUpdate } _ = do
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
                                } []
                            , Tabs.tabs {
                                activeTab
                              , boxes
                              , cacheState
                              , corpusData
                              , corpusId
                              , key: "listsLayoutWithKey-tabs-" <> (show cacheState')
                              , session
                              }
                            ] }
    where
      errorHandler = logRESTError here "[listsLayoutWithKey]"
      afterCacheStateChange cacheState = do
        launchAff_ $ clearCache unit
        sessionUpdate $ setCacheState session nodeId cacheState

type SidePanelProps =
  ( session        :: Session
  , sidePanelState :: T.Box GT.SidePanelState
  )

sidePanel :: R2.Component SidePanelProps
sidePanel = R.createElement sidePanelCpt
sidePanelCpt :: R.Component SidePanelProps
sidePanelCpt = here.component "sidePanel" cpt
  where
    cpt { session
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
  cpt { } _ = do
    -- pure $ H.h4 {} [ H.text txt ]
    pure $ H.div {} [ H.text "Hello ngrams" ]
