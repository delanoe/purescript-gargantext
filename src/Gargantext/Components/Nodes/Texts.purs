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
--------------------------------------------------------
import Gargantext.AsyncTasks as GAT
import Gargantext.Components.DocsTable as DT
import Gargantext.Components.Loader (loader)
import Gargantext.Components.NgramsTable.Loader (clearCache)
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.Nodes.Corpus (loadCorpusWithChild)
import Gargantext.Components.Nodes.Corpus.Chart.Histo (histo)
import Gargantext.Components.Nodes.Corpus.Types (CorpusData, Hyperdata(..), getCorpusInfo, CorpusInfo(..))
import Gargantext.Components.Nodes.Lists.Types as NT
import Gargantext.Components.Tab as Tab
import Gargantext.Components.Table as Table
import Gargantext.Ends (Frontends)
import Gargantext.Sessions (Session, Sessions, sessionId, getCacheState, setCacheState)
import Gargantext.Types (CTabNgramType(..), TabSubType(..), TabType(..))

thisModule :: String
thisModule = "Gargantext.Components.Nodes.Texts"
--------------------------------------------------------

type Props = (
    frontends :: Frontends
  , nodeId :: Int
  , session :: Session
  , sessionUpdate :: Session -> Effect Unit
  )

textsLayout :: Record Props -> R.Element
textsLayout props = R.createElement textsLayoutCpt props []

------------------------------------------------------------------------
textsLayoutCpt :: R.Component Props
textsLayoutCpt = R.hooksComponentWithModule thisModule "textsLayout" cpt where
  cpt { frontends, nodeId, session, sessionUpdate } _ = do
    let sid = sessionId session

    pure $ textsLayoutWithKey { frontends
                              , key: show sid <> "-" <> show nodeId
                              , nodeId
                              , session
                              , sessionUpdate }

type KeyProps = (
  key :: String
  | Props
  )

textsLayoutWithKey :: Record KeyProps -> R.Element
textsLayoutWithKey props = R.createElement textsLayoutWithKeyCpt props []

textsLayoutWithKeyCpt :: R.Component KeyProps
textsLayoutWithKeyCpt = R.hooksComponentWithModule thisModule "textsLayoutWithKey" cpt
  where
    cpt { frontends, nodeId, session, sessionUpdate } _ = do
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
            , tabs { cacheState, corpusData, corpusId, frontends, session }
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
    cacheState :: R.State NT.CacheState
  , corpusData :: CorpusData
  , corpusId :: Int
  , frontends :: Frontends
  , session :: Session
  )

tabs :: Record TabsProps -> R.Element
tabs props = R.createElement tabsCpt props []

tabsCpt :: R.Component TabsProps
tabsCpt = R.hooksComponentWithModule thisModule "tabs" cpt
  where
    cpt { cacheState, corpusId, corpusData, frontends, session } _ = do
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
                                        , tabType }

type DocViewProps a = (
    cacheState :: R.State NT.CacheState
  , corpusData :: CorpusData
  , corpusId   :: Int
  , frontends  :: Frontends
  -- , path       :: Record DT.Path
  , session    :: Session
  , tabType    :: TabSubType a
  )

docView :: forall a. Record (DocViewProps a) -> R.Element
docView props = R.createElement docViewCpt props []

docViewCpt :: forall a. R.Component (DocViewProps a)
docViewCpt = R.hooksComponentWithModule thisModule "docView" cpt
  where
    cpt props _children = do
      pure $ DT.docViewLayout $ docViewLayoutRec props

-- docViewLayoutRec :: forall a. DocViewProps a -> Record DT.LayoutProps
docViewLayoutRec { cacheState, corpusData: { defaultListId }, corpusId, frontends, session, tabType: TabDocs } =
  { cacheState
  , chart  : H.div {} []
  , corpusId: Just corpusId
  , frontends
  , listId: defaultListId
  , nodeId: corpusId
    -- ^ TODO merge nodeId and corpusId in DT
  , session
  , showSearch: true
  , tabType: TabCorpus TabDocs
  , totalRecords: 4737
  }
docViewLayoutRec { cacheState, corpusData: { defaultListId }, corpusId, frontends, session, tabType: TabMoreLikeFav } =
  { cacheState
  , chart  : H.div {} []
  , corpusId: Just corpusId
  , frontends
  , listId: defaultListId
  , nodeId: corpusId
    -- ^ TODO merge nodeId and corpusId in DT
  , session
  , showSearch: false
  , tabType: TabCorpus TabMoreLikeFav
  , totalRecords: 4737
  }
docViewLayoutRec { cacheState, corpusData: { defaultListId }, corpusId, frontends, session, tabType: TabMoreLikeTrash } =
  { cacheState
  , chart  : H.div {} []
  , corpusId: Just corpusId
  , frontends
  , listId: defaultListId
  , nodeId: corpusId
  -- ^ TODO merge nodeId and corpusId in DT
  , session
  , showSearch: false
  , tabType: TabCorpus TabMoreLikeTrash
  , totalRecords: 4737
  }
docViewLayoutRec { cacheState, corpusData: { defaultListId }, corpusId, frontends, session, tabType: TabTrash } =
  { cacheState
  , chart  : H.div {} []
  , corpusId: Nothing
  , frontends
  , listId: defaultListId
  , nodeId: corpusId
  -- ^ TODO merge nodeId and corpusId in DT
  , session
  , showSearch: true
  , tabType: TabCorpus TabTrash
  , totalRecords: 4737
  }
-- DUMMY
docViewLayoutRec { cacheState, corpusData: { defaultListId }, corpusId, frontends, session, tabType } =
  { cacheState
  , chart  : H.div {} []
  , corpusId: Nothing
  , frontends
  , listId: defaultListId
  , nodeId: corpusId
  -- ^ TODO merge nodeId and corpusId in DT
  , session
  , showSearch: true
  , tabType: TabCorpus TabTrash
  , totalRecords: 4737
  }
