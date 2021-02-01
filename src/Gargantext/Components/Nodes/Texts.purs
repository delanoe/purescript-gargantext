module Gargantext.Components.Nodes.Texts where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log, log2)
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
import Gargantext.Components.Nodes.Corpus.Document as D
import Gargantext.Components.Nodes.Corpus.Types (CorpusData, Hyperdata(..), getCorpusInfo, CorpusInfo(..))
import Gargantext.Components.Nodes.Lists.Types as NT
import Gargantext.Components.Nodes.Texts.SidePanelToggleButton (sidePanelToggleButton)
import Gargantext.Components.Nodes.Texts.Types
import Gargantext.Components.Tab as Tab
import Gargantext.Components.Table as Table
import Gargantext.Ends (Frontends)
import Gargantext.Sessions (Session, Sessions, sessionId, getCacheState, setCacheState)
import Gargantext.Types (CTabNgramType(..), Handed(..), ListId, NodeID, TabSubType(..), TabType(..))
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
        , textsProps: textProps@{ session } } _ = do
      controls <- initialControls

      pure $ Forest.forestLayoutWithTopBar forestProps
           [ topBar { controls } []
           , textsLayout (Record.merge textProps { controls }) []
           -- TODO remove className "side-panel" is preview is not triggered
           -- , H.div { className: "" }
           , H.div { className: "side-panel" }
                   [ sidePanel { controls, session } []
                   ]
           ]

--------------------------------------------------------

type TopBarProps = (
  controls :: Record TextsLayoutControls
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
        initialPath = { corpusId
                      , listId: corpusData.defaultListId
                      , limit: Nothing
                      , tabType: TabCorpus TabDocs }
        docView' path tabType = docView { cacheState
                                        , corpusData
                                        , corpusId
                                        , frontends
                                        , listId: path.listId
                                        -- , path
                                        , session
                                        , tabType
                                        , sidePanelTriggers } []

type DocViewProps a = (
    cacheState        :: R.State NT.CacheState
  , corpusData        :: CorpusData
  , corpusId          :: NodeID
  , frontends         :: Frontends
  , listId            :: ListId
  -- , path           :: Record DT.Path
  , session           :: Session
  , tabType           :: TabSubType a
  , sidePanelTriggers :: Record SidePanelTriggers
  )

docView :: forall a. R2.Component (DocViewProps a)
docView = R.createElement docViewCpt

docViewCpt :: forall a. R.Component (DocViewProps a)
docViewCpt = R.hooksComponentWithModule thisModule "docView" cpt
  where
    cpt props _children = do
      pure $ DT.docViewLayout $ docViewLayoutRec props

-- docViewLayoutRec :: forall a. DocViewProps a -> Record DT.LayoutProps
docViewLayoutRec { cacheState
                 , corpusId
                 , frontends
                 , listId
                 , session
                 , tabType: TabDocs
                 , sidePanelTriggers } =
  { cacheState
  , chart  : H.div {} []
  , frontends
  , listId
  , mCorpusId: Just corpusId
  , nodeId: corpusId
    -- ^ TODO merge nodeId and corpusId in DT
  , session
  , showSearch: true
  , sidePanelTriggers
  , tabType: TabCorpus TabDocs
  , totalRecords: 4737
  }
docViewLayoutRec { cacheState
                 , corpusId
                 , frontends
                 , listId
                 , session
                 , tabType: TabMoreLikeFav
                 , sidePanelTriggers } =
  { cacheState
  , chart  : H.div {} []
  , frontends
  , listId
  , mCorpusId: Just corpusId
  , nodeId: corpusId
    -- ^ TODO merge nodeId and corpusId in DT
  , session
  , showSearch: false
  , sidePanelTriggers
  , tabType: TabCorpus TabMoreLikeFav
  , totalRecords: 4737
  }
docViewLayoutRec { cacheState
                 , corpusId
                 , frontends
                 , listId
                 , session
                 , tabType: TabMoreLikeTrash
                 , sidePanelTriggers } =
  { cacheState
  , chart  : H.div {} []
  , frontends
  , listId
  , mCorpusId: Just corpusId
  , nodeId: corpusId
  -- ^ TODO merge nodeId and corpusId in DT
  , session
  , showSearch: false
  , sidePanelTriggers
  , tabType: TabCorpus TabMoreLikeTrash
  , totalRecords: 4737
  }
docViewLayoutRec { cacheState
                 , corpusId
                 , frontends
                 , listId
                 , session
                 , tabType: TabTrash
                 , sidePanelTriggers } =
  { cacheState
  , chart  : H.div {} []
  , frontends
  , listId
  , mCorpusId: Just corpusId
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
                 , corpusId
                 , frontends
                 , listId
                 , session
                 , tabType
                 , sidePanelTriggers } =
  { cacheState
  , chart  : H.div {} []
  , frontends
  , listId
  , mCorpusId: Just corpusId
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
  , session  :: Session
  )

sidePanel :: R2.Component SidePanelProps
sidePanel = R.createElement sidePanelCpt

sidePanelCpt :: R.Component SidePanelProps
sidePanelCpt = R.hooksComponentWithModule thisModule "sidePanel" cpt
  where
    cpt { controls: { triggers: { currentDocIdRef
                                , toggleSidePanel
                                , triggerAnnotatedDocIdChange
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

      R.useEffect3 mCorpusId mListId mNodeId $ do
        let trigger :: Record TriggerAnnotatedDocIdChangeParams -> Effect Unit
            trigger { corpusId, listId, nodeId } = do
              -- log2 "[sidePanel trigger] trigger corpusId change" corpusId
              -- log2 "[sidePanel trigger] trigger listId change" listId
              -- log2 "[sidePanel trigger] trigger nodeId change" nodeId
              if mCorpusId == Just corpusId && mListId == Just listId && mNodeId == Just nodeId && R.readRef currentDocIdRef == Just nodeId then do
                R.setRef currentDocIdRef Nothing
                R2.callTrigger toggleSidePanel unit
              else do
                setMCorpusId $ const $ Just corpusId
                setMListId $ const $ Just listId
                setMNodeId $ const $ Just nodeId
                R.setRef currentDocIdRef $ Just nodeId
                R2.callTrigger triggerSidePanel unit
        -- log2 "[sidePanel] trigger" trigger
        R2.setTrigger triggerAnnotatedDocIdChange trigger

        pure $ do
          -- log "[sidePanel] clearing triggerAnnotatedDocIdChange"
          R2.clearTrigger triggerAnnotatedDocIdChange

      let mainStyle = case fst showSidePanel of
            Opened -> { display: "block" }
            _      -> { display: "none" }

      let closeSidePanel _ = do
            R.setRef currentDocIdRef Nothing
            snd showSidePanel $ const Closed

      pure $ H.div { style: mainStyle } [
        H.div { className: "header" } [
          H.span { className: "btn btn-danger"
                 , on: { click: closeSidePanel } } [
            H.span { className: "fa fa-times" } []
          ]
        ]
      , sidePanelDocView { mCorpusId, mListId, mNodeId, session } []
      ]

type SidePanelDocView = (
    mCorpusId :: Maybe NodeID
  , mListId   :: Maybe ListId
  , mNodeId   :: Maybe NodeID
  , session   :: Session
  )

sidePanelDocView :: R2.Component SidePanelDocView
sidePanelDocView = R.createElement sidePanelDocViewCpt

sidePanelDocViewCpt :: R.Component SidePanelDocView
sidePanelDocViewCpt = R.hooksComponentWithModule thisModule "sidePanelDocView" cpt
  where
    cpt { mListId: Nothing } _ = do
      pure $ H.div {} []
    cpt { mNodeId: Nothing } _ = do
      pure $ H.div {} []
    cpt { mCorpusId
        , mListId: Just listId
        , mNodeId: Just nodeId
        , session } _ = do
      pure $ D.documentLayout { listId
                              , mCorpusId
                              , nodeId
                              , session } []
