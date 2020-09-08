module Gargantext.Components.Nodes.Texts where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Reactix as R
import Reactix.DOM.HTML as H
--------------------------------------------------------
import Gargantext.Components.DocsTable as DT
import Gargantext.Components.Loader (loader)
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.Nodes.Corpus (loadCorpusWithChild)
import Gargantext.Components.Nodes.Corpus.Chart.Histo (histo)
import Gargantext.Components.Nodes.Corpus.Types (CorpusData, Hyperdata(..), getCorpusInfo, CorpusInfo(..))
import Gargantext.Components.Tab as Tab
import Gargantext.Components.Table as Table
import Gargantext.Ends (Frontends)
import Gargantext.Sessions (Session, sessionId)
import Gargantext.Types (CTabNgramType(..), TabSubType(..), TabType(..))
import Gargantext.Utils.Reactix as R2

thisModule = "Gargantext.Components.Nodes.Texts"
--------------------------------------------------------

type Props = (
    frontends :: Frontends
  , nodeId :: Int
  , session :: Session
  )

textsLayout :: Record Props -> R.Element
textsLayout props = R.createElement textsLayoutCpt props []

------------------------------------------------------------------------
textsLayoutCpt :: R.Component Props
textsLayoutCpt = R2.hooksComponent thisModule "textsLayout" cpt where
  cpt { frontends, nodeId, session } _ = do
    let sid = sessionId session

    pure $ textsLayoutWithKey { frontends, key: show sid <> "-" <> show nodeId, nodeId, session }

type KeyProps = (
  key :: String
  | Props
  )

textsLayoutWithKey :: Record KeyProps -> R.Element
textsLayoutWithKey props = R.createElement textsLayoutWithKeyCpt props []

textsLayoutWithKeyCpt :: R.Component KeyProps
textsLayoutWithKeyCpt = R2.hooksComponent thisModule "textsLayoutWithKey" cpt
  where
    cpt { frontends, nodeId, session } _ = do
      pure $ loader {session, nodeId} loadCorpusWithChild paint
      where
        paint corpusData@{corpusId, corpusNode, defaultListId} =
          R.fragment [ Table.tableHeaderLayout headerProps, tabs' ]
          where
            NodePoly { name, date, hyperdata: Hyperdata h } = corpusNode
            CorpusInfo {desc,query,authors} = getCorpusInfo h.fields
            tabs' = tabs {session, corpusId, corpusData, frontends}
            title = "Corpus " <> name
            headerProps = { title, desc, query, date, user:authors }

data Mode = MoreLikeFav | MoreLikeTrash

derive instance genericMode :: Generic Mode _

instance showMode :: Show Mode where
  show = genericShow

derive instance eqMode :: Eq Mode

modeTabType :: Mode -> CTabNgramType
modeTabType MoreLikeFav    = CTabAuthors  -- TODO
modeTabType MoreLikeTrash  = CTabSources  -- TODO

type TabsProps = ( frontends :: Frontends, session :: Session, corpusId :: Int, corpusData :: CorpusData )

tabs :: Record TabsProps -> R.Element
tabs props = R.createElement tabsCpt props []

tabsCpt :: R.Component TabsProps
tabsCpt = R2.hooksComponent thisModule "tabs" cpt
  where
    cpt {frontends, session, corpusId, corpusData} _ = do
      (selected /\ setSelected) <- R.useState' 0
      pure $ Tab.tabs { tabs: tabs', selected }
      where
        tabs' = [ "Documents"     /\ docs,        "Trash"           /\ trash
                , "More like fav" /\ moreLikeFav, "More like trash" /\ moreLikeTrash ]
        docView' tabType = docView { frontends, session, corpusId, corpusData, tabType }
        docs = R.fragment [ docsHisto, docView' TabDocs ]
        docsHisto = histo { path, session }
          where
            path = { corpusId, listId: 0, limit: Nothing, tabType: TabCorpus TabDocs }
        moreLikeFav = docView' TabMoreLikeFav
        moreLikeTrash = docView' TabMoreLikeTrash
        trash = docView' TabTrash

type DocViewProps a =
  ( frontends :: Frontends
  , session :: Session
  , corpusId :: Int
  , corpusData :: CorpusData
  , tabType :: TabSubType a )

docView :: forall a. Record (DocViewProps a) -> R.Element
docView props = R.createElement docViewCpt props []

docViewCpt :: forall a. R.Component (DocViewProps a)
docViewCpt = R2.hooksComponent thisModule "docView" cpt
  where
    cpt {frontends, session, corpusId, corpusData: {defaultListId}, tabType} _children = do
      pure $ DT.docViewLayout $ params tabType
      where
        params :: forall b. TabSubType b -> Record DT.LayoutProps
        params TabDocs =
          { nodeId: corpusId
            -- ^ TODO merge nodeId and corpusId in DT
          , chart  : H.div {} []
          , tabType: TabCorpus TabDocs
          , totalRecords: 4737
          , listId: defaultListId
          , corpusId: Just corpusId
          , showSearch: true
          , frontends, session }
        params TabMoreLikeFav =
          { nodeId: corpusId
            -- ^ TODO merge nodeId and corpusId in DT
          , chart  : H.div {} []
          , tabType: TabCorpus TabMoreLikeFav
          , totalRecords: 4737
          , listId: defaultListId
          , corpusId: Just corpusId
          , showSearch: false
          , frontends, session }
        params TabMoreLikeTrash =
          { nodeId: corpusId
            -- ^ TODO merge nodeId and corpusId in DT
          , chart  : H.div {} []
          , tabType: TabCorpus TabMoreLikeTrash
          , totalRecords: 4737
          , listId: defaultListId
          , corpusId: Just corpusId
          , showSearch: false
          , frontends, session }
        params TabTrash =
          { nodeId: corpusId
            -- ^ TODO merge nodeId and corpusId in DT
          , chart  : H.div {} []
          , tabType: TabCorpus TabTrash
          , totalRecords: 4737
          , listId: defaultListId
          , corpusId: Nothing
          , showSearch: true
          , frontends, session }
        -- DUMMY
        params _ =
          { nodeId: corpusId
            -- ^ TODO merge nodeId and corpusId in DT
          , chart  : H.div {} []
          , tabType: TabCorpus TabTrash
          , totalRecords: 4737
          , listId: defaultListId
          , corpusId: Nothing
          , showSearch: true
          , frontends, session }
