module Gargantext.Components.Nodes.Texts where

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (launchAff_)
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Toestand as T

import Gargantext.Prelude

import Gargantext.Components.DocsTable as DT
import Gargantext.Components.NgramsTable.Loader (clearCache)
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.Nodes.Corpus (loadCorpusWithChild)
import Gargantext.Components.Nodes.Corpus.Chart.Histo (histo)
import Gargantext.Components.Nodes.Corpus.Document as D
import Gargantext.Components.Nodes.Corpus.Types (CorpusData, Hyperdata(..), getCorpusInfo, CorpusInfo(..))
import Gargantext.Components.Nodes.Lists.Types as LT
import Gargantext.Components.Nodes.Texts.Types as TT
import Gargantext.Components.Tab as Tab
import Gargantext.Components.Table as Table
import Gargantext.Ends (Frontends)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Sessions (WithSession, WithSessionContext, Session, sessionId, getCacheState)
import Gargantext.Types (CTabNgramType(..), ListId, NodeID, SidePanelState(..), TabSubType(..), TabType(..))
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Texts"

--------------------------------------------------------


type CommonPropsNoSession = (
    frontends      :: Frontends
  , nodeId         :: NodeID
  , sidePanel      :: T.Box (Maybe (Record TT.SidePanel))
  , sidePanelState :: T.Box SidePanelState
  )

type Props = WithSession CommonPropsNoSession


textsLayout :: R2.Component Props
textsLayout = R.createElement textsLayoutCpt

textsLayoutCpt :: R.Component Props
textsLayoutCpt = here.component "textsLayout" cpt where
  cpt { frontends, nodeId, session, sidePanel, sidePanelState } children = do
    pure $ textsLayoutWithKey { frontends
                              , key
                              , nodeId
                              , session
                              , sidePanel
                              , sidePanelState } children
      where
        key = show nodeId
        -- key = show sid <> "-" <> show nodeId
        --   where
        --     sid = sessionId session

type KeyProps = (
    key            :: String
  , frontends      :: Frontends
  , nodeId         :: NodeID
  , session        :: Session
  , sidePanel      :: T.Box (Maybe (Record TT.SidePanel))
  , sidePanelState :: T.Box SidePanelState
  )

textsLayoutWithKey :: R2.Component KeyProps
textsLayoutWithKey = R.createElement textsLayoutWithKeyCpt

textsLayoutWithKeyCpt :: R.Component KeyProps
textsLayoutWithKeyCpt = here.component "textsLayoutWithKey" cpt
  where
    cpt { frontends, nodeId, session, sidePanel, sidePanelState } _children = do
      cacheState <- T.useBox $ getCacheState LT.CacheOff session nodeId
      cacheState' <- T.useLive T.unequal cacheState

      R.useEffectOnce' $ do
        T.listen (\{ new } -> afterCacheStateChange new) cacheState

      useLoader { nodeId, session } loadCorpusWithChild $
        \corpusData@{ corpusId, corpusNode, defaultListId } -> do
          let NodePoly { date, hyperdata: Hyperdata h, name } = corpusNode
              CorpusInfo { authors, desc, query } = getCorpusInfo h.fields
              title = "Corpus " <> name
          R.fragment
            [ Table.tableHeaderLayout { cacheState
                                      , date
                                      , desc
                                      , query
                                      , title
                                      , user: authors
                                      , key: "textsLayoutWithKey-" <> (show cacheState') } []
            , tabs { cacheState
                   , corpusData
                   , corpusId
                   , frontends
                   , session
                   , sidePanel
                   , sidePanelState }
            ]
      where
        afterCacheStateChange cacheState = do
          launchAff_ $ clearCache unit
          -- TODO
          --sessionUpdate $ setCacheState session nodeId cacheState
          --_ <- setCacheState session nodeId cacheState

data Mode = MoreLikeFav | MoreLikeTrash

derive instance genericMode :: Generic Mode _

instance showMode :: Show Mode where
  show = genericShow

derive instance eqMode :: Eq Mode

modeTabType :: Mode -> CTabNgramType
modeTabType MoreLikeFav    = CTabAuthors  -- TODO
modeTabType MoreLikeTrash  = CTabSources  -- TODO

type TabsProps =
  ( cacheState     :: T.Box LT.CacheState
  , corpusData     :: CorpusData
  , corpusId       :: NodeID
  , frontends      :: Frontends
  , session        :: Session
  , sidePanel      :: T.Box (Maybe (Record TT.SidePanel))
  , sidePanelState :: T.Box SidePanelState
  )

tabs :: Record TabsProps -> R.Element
tabs props = R.createElement tabsCpt props []

tabsCpt :: R.Component TabsProps
tabsCpt = here.component "tabs" cpt
  where
    cpt { cacheState, corpusId, corpusData, frontends, session, sidePanel, sidePanelState } _ = do
      let path = initialPath

      activeTab <- T.useBox 0

      pure $ Tab.tabs {
          activeTab
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
                                        , sidePanel
                                        , sidePanelState } []

type DocViewProps a = (
    cacheState     :: T.Box LT.CacheState
  , corpusData     :: CorpusData
  , corpusId       :: NodeID
  , frontends      :: Frontends
  , listId         :: ListId
  -- , path        :: Record DT.Path
  , session        :: Session
  , tabType        :: TabSubType a
  , sidePanel      :: T.Box (Maybe (Record TT.SidePanel))
  , sidePanelState :: T.Box SidePanelState
  )

docView :: forall a. R2.Component (DocViewProps a)
docView = R.createElement docViewCpt

docViewCpt :: forall a. R.Component (DocViewProps a)
docViewCpt = here.component "docView" cpt
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
                 , sidePanel
                 , sidePanelState } =
  { cacheState
  , chart  : H.div {} []
  , frontends
  , listId
  , mCorpusId: Just corpusId
  , nodeId: corpusId
    -- ^ TODO merge nodeId and corpusId in DT
  , session
  , showSearch: true
  , sidePanel
  , sidePanelState
  , tabType: TabCorpus TabDocs
  , totalRecords: 4737
  }
docViewLayoutRec { cacheState
                 , corpusId
                 , frontends
                 , listId
                 , session
                 , tabType: TabMoreLikeFav
                 , sidePanel
                 , sidePanelState } =
  { cacheState
  , chart  : H.div {} []
  , frontends
  , listId
  , mCorpusId: Just corpusId
  , nodeId: corpusId
    -- ^ TODO merge nodeId and corpusId in DT
  , session
  , showSearch: false
  , sidePanel
  , sidePanelState
  , tabType: TabCorpus TabMoreLikeFav
  , totalRecords: 4737
  }
docViewLayoutRec { cacheState
                 , corpusId
                 , frontends
                 , listId
                 , session
                 , tabType: TabMoreLikeTrash
                 , sidePanel
                 , sidePanelState } =
  { cacheState
  , chart  : H.div {} []
  , frontends
  , listId
  , mCorpusId: Just corpusId
  , nodeId: corpusId
  -- ^ TODO merge nodeId and corpusId in DT
  , session
  , showSearch: false
  , sidePanel
  , sidePanelState
  , tabType: TabCorpus TabMoreLikeTrash
  , totalRecords: 4737
  }
docViewLayoutRec { cacheState
                 , corpusId
                 , frontends
                 , listId
                 , session
                 , tabType: TabTrash
                 , sidePanel
                 , sidePanelState } =
  { cacheState
  , chart  : H.div {} []
  , frontends
  , listId
  , mCorpusId: Just corpusId
  , nodeId: corpusId
  -- ^ TODO merge nodeId and corpusId in DT
  , session
  , showSearch: true
  , sidePanel
  , sidePanelState
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
                 , sidePanel
                 , sidePanelState } =
  { cacheState
  , chart  : H.div {} []
  , frontends
  , listId
  , mCorpusId: Just corpusId
  , nodeId: corpusId
  -- ^ TODO merge nodeId and corpusId in DT
  , session
  , showSearch: true
  , sidePanel
  , sidePanelState
  , tabType: TabCorpus TabTrash
  , totalRecords: 4737
  }


--------------------------------------------------------
type SidePanelProps = (
    session        :: Session
  , sidePanel      :: T.Box (Maybe (Record TT.SidePanel))
  , sidePanelState :: T.Box SidePanelState
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
      sidePanel' <- T.useLive T.unequal sidePanel

      -- R.useEffect' $ do
      --   let toggleSidePanel' _  = snd sidePanelState toggleSidePanelState
      --       triggerSidePanel' _ = snd sidePanelState $ const Opened
      --   R2.setTrigger toggleSidePanel  toggleSidePanel'
      --   R2.setTrigger triggerSidePanel triggerSidePanel'

      -- (mCorpusId /\ setMCorpusId) <- R.useState' Nothing
      -- (mListId /\ setMListId) <- R.useState' Nothing
      -- (mNodeId /\ setMNodeId) <- R.useState' Nothing

      -- R.useEffect3 mCorpusId mListId mNodeId $ do
      --   if mCorpusId == Just corpusId && mListId == Just listId && mNodeId == Just nodeId && mCurrentDocId == Just nodeId then do
      --     T.modify_ (\sp -> sp { mCurrentDocId = Nothing }) sidePanel
      --   else do
      --     T.modify_ (\sp -> sp { mCorpusId = Just corpusId
      --                         , mCurrentDocId = Just nodeId
      --                         , mListId = Just listId
      --                         , mNodeId = Just nodeId }) sidePanel
        -- let trigger :: Record TriggerAnnotatedDocIdChangeParams -> Effect Unit
        --     trigger { corpusId, listId, nodeId } = do
              -- log2 "[sidePanel trigger] trigger corpusId change" corpusId
              -- log2 "[sidePanel trigger] trigger listId change" listId
              -- log2 "[sidePanel trigger] trigger nodeId change" nodeId
              -- if mCorpusId == Just corpusId && mListId == Just listId && mNodeId == Just nodeId && mCurrentDocId == Just nodeId then do
                -- R.setRef currentDocIdRef Nothing
                -- T.modify_ (\sp -> sp { mCurrentDocId = Nothing }) sidePanel
                -- R2.callTrigger toggleSidePanel unit
              -- else do
                -- setMCorpusId $ const $ Just corpusId
                -- setMListId $ const $ Just listId
                -- setMNodeId $ const $ Just nodeId
                -- R.setRef currentDocIdRef $ Just nodeId
                -- R2.callTrigger triggerSidePanel unit
                -- T.modify_ (\sp -> sp { mCorpusId = Just corpusId
                --                     , mCurrentDocId = Just nodeId
                --                     , mListId = Just listId
                --                     , mNodeId = Just nodeId }) sidePanel
        -- log2 "[sidePanel] trigger" trigger
        -- R2.setTrigger triggerAnnotatedDocIdChange trigger
        -- pure unit

        -- pure $ do
        --   -- log "[sidePanel] clearing triggerAnnotatedDocIdChange"
        --   R2.clearTrigger triggerAnnotatedDocIdChange

      let mainStyle = case sidePanelState' of
            Opened -> { display: "block" }
            _      -> { display: "none" }

      let closeSidePanel _ = do
            -- T.modify_ (\sp -> sp { mCurrentDocId = Nothing
            --                     , state = Closed }) sidePanel
            T.write_ Closed sidePanelState
            T.write_ Nothing sidePanel

      pure $ H.div { style: mainStyle } [
        H.div { className: "header" } [
          H.span { className: "btn btn-danger"
                 , on: { click: closeSidePanel } } [
            H.span { className: "fa fa-times" } []
          ]
        ]
      , sidePanelDocView { mSidePanel: sidePanel', session } []
      ]

type SidePanelDocView = (
    mSidePanel :: Maybe (Record TT.SidePanel)
  , session    :: Session
  )

sidePanelDocView :: R2.Component SidePanelDocView
sidePanelDocView = R.createElement sidePanelDocViewCpt

sidePanelDocViewCpt :: R.Component SidePanelDocView
sidePanelDocViewCpt = here.component "sidePanelDocView" cpt
  where
    cpt { mSidePanel: Nothing } _ = do
      pure $ H.div {} []
    cpt { mSidePanel: Just { corpusId, listId, nodeId }
        , session } _ = do
      pure $ D.documentLayout { listId
                              , mCorpusId: Just corpusId
                              , nodeId
                              , session } []
