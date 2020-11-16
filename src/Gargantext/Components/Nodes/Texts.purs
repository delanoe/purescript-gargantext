module Gargantext.Components.Nodes.Texts where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
--------------------------------------------------------
import Gargantext.AsyncTasks as GAT
import Gargantext.Components.DocsTable as DT
import Gargantext.Components.Forest as Forest
import Gargantext.Components.Loader (loader)
import Gargantext.Components.NgramsTable.Loader (clearCache)
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.Nodes.Corpus (loadCorpusWithChild)
import Gargantext.Components.Nodes.Corpus.Chart.Histo (histo)
import Gargantext.Components.Nodes.Corpus.Types (CorpusData, Hyperdata(..), getCorpusInfo, CorpusInfo(..))
import Gargantext.Components.Nodes.Lists.Types as NT
import Gargantext.Components.Nodes.Texts.SidePanelToggleButton (sidePanelToggleButton)
import Gargantext.Components.Nodes.Texts.Types
import Gargantext.Components.Tab as Tab
import Gargantext.Components.Table as Table
import Gargantext.Ends (Frontends)
import Gargantext.Sessions (Session, Sessions, sessionId, getCacheState, setCacheState)
import Gargantext.Types (CTabNgramType(..), Handed(..), NodeID, ReloadS, TabSubType(..), TabType(..))
import Gargantext.Utils.Reactix as R2

thisModule :: String
thisModule = "Gargantext.Components.Nodes.Texts"

--------------------------------------------------------
type TextsWithForest = (
    forestProps :: Record Forest.ForestLayoutProps
  , textsProps  :: Record CommonProps
  )

textsWithForest :: R2.Component TextsWithForest
textsWithForest = R.createElement textsWithForestCpt

textsWithForestCpt :: R.Component TextsWithForest
textsWithForestCpt = R.hooksComponentWithModule thisModule "textsWithForest" cpt
  where
    cpt { forestProps
        , textsProps } _ = do
      controls <- initialControls

      pure $ Forest.forestLayoutWithTopBar forestProps [
        topBar { controls } []
      , H.div {className: "col-md-10"} [
          H.div {id: "page-wrapper"} [
            H.div {className: "container-fluid"} [
              textsLayout (Record.merge textsProps { controls }) []
            ]
          ]
        ]
      , sidePanel { controls } []
      ]

--------------------------------------------------------

type TextsLayoutControls = (
    showSidePanel :: R.State SidePanelState
  , triggers      :: Record SidePanelTriggers
  )

initialControls :: R.Hooks (Record TextsLayoutControls)
initialControls = do
  showSidePanel  <- R.useState' InitialClosed
  triggers <- emptySidePanelTriggers

  pure $ {
      showSidePanel
    , triggers
  }

type TopBarProps = (
  controls :: Record TextsLayoutControls
  )

topBar :: R2.Component TopBarProps
topBar = R.createElement topBarCpt

topBarCpt :: R.Component TopBarProps
topBarCpt = R.hooksComponentWithModule thisModule "topBar" cpt
  where
    cpt { controls } _ = do
      pure $
        H.ul { className: "nav navbar-nav" } [
          H.li {} [
             sidePanelToggleButton { state: controls.showSidePanel } []
             ]
          ]  -- head (goes to top bar)

------------------------------------------------------------------------
type CommonProps = (
    frontends     :: Frontends
  , nodeId        :: Int
  , session       :: Session
  , sessionUpdate :: Session -> Effect Unit
  )

type Props = (
    controls       :: Record TextsLayoutControls
  | CommonProps
  )

textsLayout :: R2.Component Props
textsLayout = R.createElement textsLayoutCpt

textsLayoutCpt :: R.Component Props
textsLayoutCpt = R.hooksComponentWithModule thisModule "textsLayout" cpt
  where
    cpt { controls, frontends, nodeId, session, sessionUpdate } children = do
      let sid = sessionId session

      pure $ textsLayoutWithKey { controls
                                , frontends
                                , key: show sid <> "-" <> show nodeId
                                , nodeId
                                , session
                                , sessionUpdate } children

type KeyProps = (
  key :: String
  | Props
  )

textsLayoutWithKey :: R2.Component KeyProps
textsLayoutWithKey = R.createElement textsLayoutWithKeyCpt

textsLayoutWithKeyCpt :: R.Component KeyProps
textsLayoutWithKeyCpt = R.hooksComponentWithModule thisModule "textsLayoutWithKey" cpt
  where
    cpt { controls, frontends, nodeId, session, sessionUpdate } _children = do
      cacheState <- R.useState' $ getCacheState NT.CacheOff session nodeId

      pure $ loader { nodeId, session } loadCorpusWithChild $
        \corpusData@{ corpusId, corpusNode, defaultListId } -> do
          let NodePoly { date, hyperdata: Hyperdata h, name } = corpusNode
              CorpusInfo { authors, desc, query } = getCorpusInfo h.fields
              title = "Corpus " <> name

          R.fragment [
              Table.tableHeaderLayout { afterCacheStateChange
                                      , cacheState
                                      , date
                                      , desc
                                      , key: "textsLayoutWithKey-" <> (show $ fst cacheState)
                                      , query
                                      , title
                                      , user: authors }
            , tabs { cacheState
                   , corpusData
                   , corpusId
                   , frontends
                   , session
                   , sidePanelTriggers: controls.triggers }
          ]
      where
        afterCacheStateChange cacheState = do
          launchAff_ $ clearCache unit
          sessionUpdate $ setCacheState session nodeId cacheState

data Mode = MoreLikeFav | MoreLikeTrash

derive instance genericMode :: Generic Mode _

instance showMode :: Show Mode where
  show = genericShow

derive instance eqMode :: Eq Mode

modeTabType :: Mode -> CTabNgramType
modeTabType MoreLikeFav    = CTabAuthors  -- TODO
modeTabType MoreLikeTrash  = CTabSources  -- TODO

type TabsProps = (
    cacheState      :: R.State NT.CacheState
  , corpusData      :: CorpusData
  , corpusId        :: Int
  , frontends       :: Frontends
  , session         :: Session
  , sidePanelTriggers :: Record SidePanelTriggers
  )

tabs :: Record TabsProps -> R.Element
tabs props = R.createElement tabsCpt props []

tabsCpt :: R.Component TabsProps
tabsCpt = R.hooksComponentWithModule thisModule "tabs" cpt
  where
    cpt { cacheState, corpusId, corpusData, frontends, session, sidePanelTriggers } _ = do
      (selected /\ setSelected) <- R.useState' 0

      let path = initialPath

      pure $ Tab.tabs {
          selected
        , tabs: [
            "Documents"       /\ R.fragment [
                histo { path, session }
              , docView' path TabDocs
              ]
          , "Trash"           /\ docView' path TabTrash
          -- , "More like fav"   /\ docView' path TabMoreLikeFav
          -- , "More like trash" /\ docView' path TabMoreLikeTrash
          ]
        }

      where
        initialPath = { corpusId, listId: 0, limit: Nothing, tabType: TabCorpus TabDocs }
        docView' path tabType = docView { cacheState
                                        , corpusData
                                        , corpusId
                                        , frontends
                                        -- , path
                                        , session
                                        , tabType
                                        , sidePanelTriggers }

type DocViewProps a = (
    cacheState      :: R.State NT.CacheState
  , corpusData      :: CorpusData
  , corpusId        :: Int
  , frontends       :: Frontends
  -- , path         :: Record DT.Path
  , session         :: Session
  , tabType         :: TabSubType a
  , sidePanelTriggers :: Record SidePanelTriggers
  )

docView :: forall a. Record (DocViewProps a) -> R.Element
docView props = R.createElement docViewCpt props []

docViewCpt :: forall a. R.Component (DocViewProps a)
docViewCpt = R.hooksComponentWithModule thisModule "docView" cpt
  where
    cpt props _children = do
      pure $ DT.docViewLayout $ docViewLayoutRec props

-- docViewLayoutRec :: forall a. DocViewProps a -> Record DT.LayoutProps
docViewLayoutRec { cacheState
                 , corpusData: { defaultListId }
                 , corpusId
                 , frontends
                 , session
                 , tabType: TabDocs
                 , sidePanelTriggers } =
  { cacheState
  , chart  : H.div {} []
  , corpusId: Just corpusId
  , frontends
  , listId: defaultListId
  , nodeId: corpusId
    -- ^ TODO merge nodeId and corpusId in DT
  , session
  , showSearch: true
  , sidePanelTriggers
  , tabType: TabCorpus TabDocs
  , totalRecords: 4737
  }
docViewLayoutRec { cacheState
                 , corpusData: { defaultListId }
                 , corpusId
                 , frontends
                 , session
                 , tabType: TabMoreLikeFav
                 , sidePanelTriggers } =
  { cacheState
  , chart  : H.div {} []
  , corpusId: Just corpusId
  , frontends
  , listId: defaultListId
  , nodeId: corpusId
    -- ^ TODO merge nodeId and corpusId in DT
  , session
  , showSearch: false
  , sidePanelTriggers
  , tabType: TabCorpus TabMoreLikeFav
  , totalRecords: 4737
  }
docViewLayoutRec { cacheState
                 , corpusData: { defaultListId }
                 , corpusId
                 , frontends
                 , session
                 , tabType: TabMoreLikeTrash
                 , sidePanelTriggers } =
  { cacheState
  , chart  : H.div {} []
  , corpusId: Just corpusId
  , frontends
  , listId: defaultListId
  , nodeId: corpusId
  -- ^ TODO merge nodeId and corpusId in DT
  , session
  , showSearch: false
  , sidePanelTriggers
  , tabType: TabCorpus TabMoreLikeTrash
  , totalRecords: 4737
  }
docViewLayoutRec { cacheState
                 , corpusData: { defaultListId }
                 , corpusId
                 , frontends
                 , session
                 , tabType: TabTrash
                 , sidePanelTriggers } =
  { cacheState
  , chart  : H.div {} []
  , corpusId: Nothing
  , frontends
  , listId: defaultListId
  , nodeId: corpusId
  -- ^ TODO merge nodeId and corpusId in DT
  , session
  , showSearch: true
  , sidePanelTriggers
  , tabType: TabCorpus TabTrash
  , totalRecords: 4737
  }
-- DUMMY
docViewLayoutRec { cacheState
                 , corpusData: { defaultListId }
                 , corpusId
                 , frontends
                 , session
                 , tabType
                 , sidePanelTriggers } =
  { cacheState
  , chart  : H.div {} []
  , corpusId: Nothing
  , frontends
  , listId: defaultListId
  , nodeId: corpusId
  -- ^ TODO merge nodeId and corpusId in DT
  , session
  , showSearch: true
  , sidePanelTriggers
  , tabType: TabCorpus TabTrash
  , totalRecords: 4737
  }


--------------------------------------------------------

type SidePanelProps = (
  controls :: Record TextsLayoutControls
  )

sidePanel :: R2.Component SidePanelProps
sidePanel = R.createElement sidePanelCpt

sidePanelCpt :: R.Component SidePanelProps
sidePanelCpt = R.hooksComponentWithModule thisModule "sidePanel" cpt
  where
    cpt { controls: { showSidePanel: (InitialClosed /\ _) } } _ = do
      pure $ H.div {} []
    cpt { controls: { showSidePanel: (Closed /\ _) } } _ = do
      pure $ H.div {} []
    cpt { controls: { showSidePanel: (Opened /\ _) } } _ = do
      pure $ H.div { className: "side-bar" } [
        H.h4 {} [ H.text "Side Bar" ]
        ]
