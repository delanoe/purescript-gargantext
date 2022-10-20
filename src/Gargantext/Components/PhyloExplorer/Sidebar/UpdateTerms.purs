module Gargantext.Components.PhyloExplorer.Sidebar.UpdateTerms where

import Gargantext.Prelude

import Data.Array as A
import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Gargantext.Components.App.Store as AppStore
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), ComponentStatus(..), Variant(..))
import Gargantext.Components.PhyloExplorer.Store as PhyloStore
import Gargantext.Components.PhyloExplorer.Types (ListId, CorpusId)
import Gargantext.Config.REST (AffRESTError)
import Gargantext.Core.NgramsTable.Functions as NTC
import Gargantext.Core.NgramsTable.Types as CNT
import Gargantext.Hooks.Session (useSession)
import Gargantext.Sessions (Session)
import Gargantext.Types (CTabNgramType, FrontendError(..), TabSubType(..), TabType(..), TermList(..))
import Gargantext.Utils ((?))
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.PhyloExplorer.Sidebar.UpdateTerms"

type Props =
  ( selectedTerm    :: String
  , ngramType       :: CTabNgramType
   )

-- | @NOTE #408: only dealing with single Term selection
-- |             (hence not dealing with multiple selection, nor Branch,
-- |             nor Source → if so, please change the source code accordingly)
updateTerms :: R2.Leaf Props
updateTerms = R2.leaf updateTermsCpt

updateTermsCpt :: R.Component Props
updateTermsCpt = here.component "main" cpt where
  cpt { selectedTerm
      , ngramType
      } _ = do
    -- | States
    -- |
    session <- useSession

    { errors
    , reloadForest
    } <- AppStore.use

    store <- PhyloStore.use

    corpusId     <- R2.useLive' store.corpusId
    listId       <- R2.useLive' store.listId

    onPending' /\ onPending <- R2.useBox' false

    -- | Behaviors
    -- |
    let
      onClick :: TermList -> Effect Unit
      onClick termList = launchAff_ do

        liftEffect do
          T.write_ true onPending

        res <- sendPatch
          termList
          session
          corpusId
          listId
          ngramType
          selectedTerm

        case res of
          Left err -> liftEffect do
            T.modify_ (A.cons $ FRESTError { error: err }) errors
            here.warn2 "[sendPatches] RESTError" err
          Right _ -> liftEffect do
            T2.reload reloadForest

        liftEffect do
          T.write_ false onPending

    -- | Render
    -- |
    pure $

      B.buttonGroup
      { collapse: false }
      [
        B.button
        { variant: ButtonVariant Light
        , status: onPending' ?
            Disabled $
            Enabled
        , callback: const $ onClick CandidateTerm
        }
        [
          B.icon
          { name: "circle"
          , className: "mr-1 candidate-term"
          }
        ,
          H.text "Move as candidate"
        ]
      ,
        B.button
        { variant: ButtonVariant Light
        , status: onPending' ?
            Disabled $
            Enabled
        , callback: const $ onClick StopTerm
        }
        [
          B.icon
          { name: "circle"
          , className: "mr-1 stop-term"
          }
        ,
          H.text "Move as stop"
        ]
      ]

sendPatch ::
     TermList
  -> Session
  -> CorpusId
  -> ListId
  -> CTabNgramType
  -> String
  -> AffRESTError CNT.VersionedNgramsPatches
sendPatch termList session corpusId listId tabNgramType label
    = NTC.putNgramsPatches coreParams versioned
  -- (!) in the future, as the like of Graph update terms, handling task
  -- >>= case _ of
      -- Left err -> pure $ Left err
      -- Right ret -> do
        -- _task <- NTC.postNgramsChartsAsync coreParams
        -- pure $ Right ret
  where
    -- @NOTE #408: currently no versioning for Phylo
    versioned :: CNT.VersionedNgramsPatches
    versioned
      = CNT.Versioned
          { version: 1
          , data: np
          }

    coreParams :: CNT.CoreParams ()
    coreParams
      = { session
        , nodeId: corpusId
        , listIds: [ listId ]
        , tabType: TabCorpus (TabNgramType tabNgramType)
        }

    term :: CNT.NgramsTerm
    term = NTC.normNgram tabNgramType label

    np :: CNT.NgramsPatches
    np = NTC.singletonPatchMap term $ CNT.NgramsPatch
          { patch_children: mempty
          , patch_list
          }

    patch_list :: CNT.Replace TermList
    patch_list
      = CNT.Replace
          { new: termList
          , old: MapTerm
          }
