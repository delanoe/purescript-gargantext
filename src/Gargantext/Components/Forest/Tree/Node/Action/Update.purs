module Gargantext.Components.Forest.Tree.Node.Action.Update where

import Gargantext.Components.Forest.Tree.Node.Action.Update.Types
import Gargantext.Prelude

import DOM.Simple.Console (log3)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Gargantext.Components.Bootstrap.Types (ComponentStatus(..))
import Gargantext.Components.Forest.Tree.Node.Action.Types (Action(..))
import Gargantext.Components.Forest.Tree.Node.Tools (formChoiceSafe, submitButton, panel)
import Gargantext.Components.PhyloExplorer.API as Phylo
import Gargantext.Components.PhyloExplorer.ConfigForm as PhyloForm
import Gargantext.Components.PhyloExplorer.ConfigFormParser as PhyloHook
import Gargantext.Config.REST (RESTError, AffRESTError)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, post)
import Gargantext.Types (ID, NodeType(..))
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Record (merge)
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Action.Update"

updateRequest :: UpdateNodeParams -> Session -> ID -> AffRESTError GT.AsyncTaskWithType
updateRequest updateNodeParams session nodeId = do
  eTask :: Either RESTError GT.AsyncTask <- post session p updateNodeParams
  case eTask of
    Left err -> pure $ Left err
    Right task -> pure $ Right $ GT.AsyncTaskWithType { task, typ: GT.UpdateNode }
    where
      p = GR.NodeAPI GT.Node (Just nodeId) "update"

----------------------------------------------------------------------
type UpdateProps =
  ( dispatch :: Action -> Aff Unit
  , nodeType :: NodeType )

update ::  R2.Component UpdateProps
update = R.createElement updateCpt
updateCpt :: R.Component UpdateProps
updateCpt = here.component "update" cpt where
  cpt props@{ nodeType: Dashboard } _ = pure $ updateDashboard props []
  cpt props@{ nodeType: Graph     } _ = pure $ updateGraph props []
  cpt props@{ nodeType: NodeList  } _ = pure $ updateNodeList props []
  cpt props@{ nodeType: NodeTexts } _ = pure $ updateTexts props []
  cpt props@{ nodeType: Phylo     } _ = pure $ updatePhylo props
  cpt props@{ nodeType: _         } _ = pure $ updateOther props []

updateDashboard :: R2.Component UpdateProps
updateDashboard = R.createElement updateDashboardCpt
updateDashboardCpt :: R.Component UpdateProps
updateDashboardCpt = here.component "updateDashboard" cpt where
  cpt { dispatch } _ = do
    methodBoard <- T.useBox All
    methodBoard' <- T.useLive T.unequal methodBoard

    pure $ panel [ -- H.text "Update with"
                  formChoiceSafe { items: [All, Sources, Authors, Institutes, Ngrams]
                                 , default: methodBoard'
                                 , callback: \val -> T.write_ val methodBoard
                                 , print: show } []
                 ]
                 (submitButton (UpdateNode $ UpdateNodeParamsBoard { methodBoard: methodBoard' }) dispatch)

updateGraph :: R2.Component UpdateProps
updateGraph = R.createElement updateGraphCpt
updateGraphCpt :: R.Component UpdateProps
updateGraphCpt = here.component "updateGraph" cpt where
  cpt { dispatch } _ = do
    methodGraphMetric <- T.useBox Order1
    methodGraphMetric' <- T.useLive T.unequal methodGraphMetric

    methodGraphClustering <- T.useBox Spinglass
    methodGraphClustering' <- T.useLive T.unequal methodGraphClustering

    pure $ panel [ -- H.text "Update with"
                  formChoiceSafe { items: [Order1, Order2]
                                 , default: methodGraphMetric'
                                 , callback: \val -> T.write_ val methodGraphMetric
                                 , print: show } []
                 , formChoiceSafe { items: [Spinglass, Confluence]
                                 , default: methodGraphClustering'
                                 , callback: \val -> T.write_ val methodGraphClustering
                                 , print: show } []

                 ]
                 (submitButton (UpdateNode $ UpdateNodeParamsGraph { methodGraphMetric: methodGraphMetric'
                                                                   , methodGraphClustering: methodGraphClustering'
                                                                   }
                               ) dispatch
                  )



updatePhylo :: R2.Leaf UpdateProps
updatePhylo = R2.leaf updatePhyloCpt
updatePhyloCpt :: R.Component UpdateProps
updatePhyloCpt = here.component "updatePhylo" cpt where
  cpt { dispatch } _ = do
  -- Hooks
    parser <- PhyloHook.useConfigFormParser

  -- Helpers
    let
      -- @NOTE #219: use a config property returned by GET phylo route
      defaultData :: Phylo.UpdateData
      defaultData = Phylo.UpdateData
        { proximity: 0.5
        , synchrony: 0.5
        , quality: 0.5
        , exportFilter: 0.5
        , timeUnit: Phylo.Year $ Phylo.TimeUnitCriteria
          { period: 3
          , step: 1
          , matchingFrame: 5
          }
        , clique: Phylo.MaxClique
            { size: 5
            , threshold: 0.0001
            , filter: Phylo.ByThreshold
            }
        }

  -- Behaviors

      onSubmit :: Record PhyloForm.FormData -> Effect Unit
      onSubmit r = case parser.fromFormData r of
        Left error -> log3 "[handleFormError]" error r
        Right r'   -> do
          opts <- pure $ options r'
          launchAff_ $ dispatch opts

        where
          options :: Phylo.UpdateData -> Action
          options params
            = UpdateNode $ UpdateNodeParamsPhylo
              { methodPhylo: params
              }

  -- Render
    pure $
      PhyloForm.configForm $
      { callback: onSubmit
      , status: Enabled
      } `merge` parser.toFormData defaultData

updateNodeList :: R2.Component UpdateProps
updateNodeList = R.createElement updateNodeListCpt
updateNodeListCpt :: R.Component UpdateProps
updateNodeListCpt = here.component "updateNodeList" cpt where
  cpt { dispatch } _ = do
    methodList <- T.useBox Basic
    methodList' <- T.useLive T.unequal methodList

    pure $ panel [ -- H.text "Update with"
                  formChoiceSafe { items: [Basic, Advanced, WithModel]
                                 , default: methodList'
                                 , callback: \val -> T.write_ val methodList
                                 , print: show } []
                 ]
                 (submitButton (UpdateNode $ UpdateNodeParamsList { methodList: methodList' }) dispatch)

updateTexts :: R2.Component UpdateProps
updateTexts = R.createElement updateTextsCpt
updateTextsCpt :: R.Component UpdateProps
updateTextsCpt = here.component "updateTexts" cpt where
  cpt { dispatch } _ = do
    methodTexts <- T.useBox NewNgrams
    methodTexts' <- T.useLive T.unequal methodTexts

    pure $ panel [ -- H.text "Update with"
                  formChoiceSafe { items: [NewNgrams, NewTexts, Both]
                                 , default: methodTexts'
                                 , callback: \val -> T.write_ val methodTexts
                                 , print: show } []
                 ]
                 (submitButton (UpdateNode $ UpdateNodeParamsTexts { methodTexts: methodTexts' }) dispatch)

updateOther :: R2.Component UpdateProps
updateOther = R.createElement updateOtherCpt
updateOtherCpt :: R.Component UpdateProps
updateOtherCpt = here.component "updateOther" cpt where
  cpt _ _ = do
    pure $ H.div {} []

-- fragmentPT $ "Update " <> show nodeType
