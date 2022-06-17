module Gargantext.Components.Nodes.Lists.Tabs where

import Gargantext.Components.Nodes.Lists.Types hiding (here)

import Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Gargantext.Components.App.Store (Boxes)
import Gargantext.Components.NgramsTable as NT
import Gargantext.Components.NgramsTable.Core (PageParams)
import Gargantext.Components.NgramsTable.Core as NTC
import Gargantext.Components.Nodes.Corpus.Chart.Metrics (metrics)
import Gargantext.Components.Nodes.Corpus.Chart.Pie (pie, bar)
import Gargantext.Components.Nodes.Corpus.Chart.Tree (tree)
import Gargantext.Components.Nodes.Corpus.Chart.Utils (mNgramsTypeFromTabType)
import Gargantext.Components.Nodes.Corpus.Types (CorpusData)
import Gargantext.Components.Tab as Tab
import Gargantext.Components.Table.Types (Params)
import Gargantext.Prelude (bind, pure, unit, ($))
import Gargantext.Sessions (Session)
import Gargantext.Types (CTabNgramType(..), Mode(..), TabSubType(..), TabType(..), modeTabType)
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Record as Record
import Record.Extra as RX
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Lists.Tabs"

type Props = (
    activeTab  :: T.Box Int
  , boxes      :: Boxes
  , cacheState :: T.Box CacheState
  , corpusData :: CorpusData
  , corpusId   :: Int
  , session    :: Session
  )

tabs :: Record ( key :: String | Props ) -> R.Element
tabs props = R.createElement tabsCpt props []
tabsCpt :: R.Component ( key :: String | Props )
tabsCpt = here.component "tabs" cpt where
  cpt props@{ activeTab } _ = do
    pure $ Tab.tabs { activeTab
                    , tabs: tabs' } where
      tabs' = [ "Terms"      /\ view Terms []
              , "Authors"    /\ view Authors []
              , "Institutes" /\ view Institutes []
              , "Sources"    /\ view Sources []
              ]
      common = RX.pick props :: Record Props
      view mode = tab $ Record.merge common { mode }

type TabProps = ( mode :: Mode | Props )

tab :: R2.Component TabProps
tab = R.createElement tabCpt

tabCpt :: R.Component TabProps
tabCpt = here.component "tab" cpt where
  cpt props _ = do
    path <- T.useBox $ NTC.initialPageParams props.session initialPath.corpusId [initialPath.listId] initialPath.tabType
    pure $ ngramsView (Record.merge props { path }) []
    where
      tabNgramType = modeTabType props.mode
      tabType      = TabCorpus (TabNgramType tabNgramType)
      listId       = props.corpusData.defaultListId
      corpusId     = props.corpusId
      initialPath  = { corpusId
                      -- , limit: Just 1000
                      , listId
                      , tabType
                      }
      

type NgramsViewProps = ( path :: T.Box PageParams | TabProps )

ngramsView :: R2.Component NgramsViewProps
ngramsView = R.createElement ngramsViewCpt
ngramsViewCpt :: R.Component NgramsViewProps
ngramsViewCpt = here.component "ngramsView" cpt where
  cpt props@{ boxes
            , cacheState
            , corpusData: { defaultListId }
            , mode
            , session
            , path } _ = do
      chartsReload <- T.useBox T2.newReload
      onCancelRef <- R.useRef Nothing
      onNgramsClickRef <- R.useRef Nothing
      onSaveRef <- R.useRef Nothing
      treeEditBox <- T.useBox NT.initialTreeEdit

      { listIds, nodeId, params } <- T.useLive T.unequal path

      pure $
        R.fragment
        [
          ngramsView'
          { boxes
          , corpusData: props.corpusData
          , listIds
          , mode
          , nodeId
          , params
          , session
          } []
        ,
          NT.mainNgramsTable
          { afterSync: afterSync chartsReload
          , boxes
          , cacheState
          , defaultListId
          , path
          , session
          , tabNgramType
          , tabType
          , treeEdit: { box: treeEditBox
                      , getNgramsChildren: NT.getNgramsChildrenAff session nodeId listIds tabType
                      , onCancelRef
                      , onNgramsClickRef
                      , onSaveRef }
          , withAutoUpdate: false
          } []
        ]
      where
        afterSync chartsReload _ = do
          case mNgramsType of
            Just _ -> do
              -- NOTE: No need to recompute chart, after ngrams are sync this
              -- should be recomputed already
              -- We just refresh it
              -- _ <- recomputeChart session chartType ngramsType corpusId listId
              liftEffect $ T2.reload chartsReload
            Nothing         -> pure unit

        tabNgramType = modeTabType mode
        tabType      = TabCorpus (TabNgramType tabNgramType)
        mNgramsType  = mNgramsTypeFromTabType tabType



----------------


-- @XXX re-render issue -> clone component
type NgramsViewProps' =
  ( boxes         :: Boxes
  , corpusData    :: CorpusData
  , listIds       :: Array Int
  , mode          :: Mode
  , nodeId        :: Int
  , params        :: Params
  , session       :: Session
  )

ngramsView' :: R2.Component NgramsViewProps'
ngramsView' = R.createElement ngramsViewCpt'
--ngramsViewCpt' :: R.Memo NgramsViewProps'
--ngramsViewCpt' = R.memo' $ here.component "ngramsView_clone" cpt where
ngramsViewCpt' :: R.Component NgramsViewProps'
ngramsViewCpt' = here.component "ngramsView_clone" cpt where
  cpt { boxes
      , corpusData: { defaultListId }
      , listIds
      , mode
      , nodeId
      , params
      , session
      } _ = do

    let path' = {
      corpusId: nodeId
    , limit: params.limit
    , listId: fromMaybe defaultListId $ A.head listIds
    , tabType: tabType
    }

    let chartParams = {
      corpusId: path'.corpusId
    , limit: Just path'.limit
    , listId: path'.listId
    , tabType: path'.tabType
    }

    pure $

      R.fragment $
      charts chartParams tabNgramType

    where
      charts _params CTabTerms = [
          {-
          H.div {className: "row"}
                [ H.div {className: "col-12 d-flex justify-content-center"}
                  [ H.img { src: "images/Gargantextuel-212x300.jpg"
                          , id: "funnyimg"
                        }
                  ]
                ]

              R2.select { className: "form-control"
                        , defaultValue: show chartType
                        , on: { change: \e -> setChartType
                                             $ const
                                             $ fromMaybe Histo
                                             $ chartTypeFromString
                                             $ R.unsafeEventValue e
                              }
                        } [
                H.option { value: show Histo     } [ H.text $ show Histo     ]
              , H.option { value: show Scatter   } [ H.text $ show Scatter   ]
              , H.option { value: show ChartBar  } [ H.text $ show ChartBar  ]
              , H.option { value: show ChartPie  } [ H.text $ show ChartPie  ]
              , H.option { value: show ChartTree } [ H.text $ show ChartTree ]
              ]
            ]
          ]
        , getChartFunction chartType $ { path: params, session }
        -}
      ]
      charts params' _        = [ chart params' mode ]

      chart path Authors    = pie     { boxes, path, session, onClick: Nothing, onInit: Nothing }
      chart path Institutes = tree    { boxes, path, session, onClick: Nothing, onInit: Nothing }
      chart path Sources    = bar     { boxes, path, session, onClick: Nothing, onInit: Nothing }
      chart path Terms      = metrics { boxes, path, session, onClick: Nothing, onInit: Nothing }

      tabType      = TabCorpus (TabNgramType tabNgramType)

      tabNgramType = modeTabType mode
