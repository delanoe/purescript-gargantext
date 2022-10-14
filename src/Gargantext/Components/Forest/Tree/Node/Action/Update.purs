module Gargantext.Components.Forest.Tree.Node.Action.Update where

import Gargantext.Prelude

import Gargantext.Components.Forest.Tree.Node.Action.Update.Types (Charts(..), Granularity(..), GraphMetric(..), Method(..), PartitionMethod(..), UpdateNodeParams(..), Strength(..))

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

    methodGraphEdgesStrength  <- T.useBox Strong
    methodGraphEdgesStrength' <- T.useLive T.unequal methodGraphEdgesStrength

    methodGraphNodeType1  <- T.useBox GT.CTabTerms
    methodGraphNodeType1' <- T.useLive T.unequal methodGraphNodeType1

    methodGraphNodeType2  <- T.useBox GT.CTabTerms
    methodGraphNodeType2' <- T.useLive T.unequal methodGraphNodeType2

    methodGraphClustering <- T.useBox Spinglass
    methodGraphClustering' <- T.useLive T.unequal methodGraphClustering

    let
      callback :: Action -> Aff Unit
      callback = dispatch >=> \_ -> dispatch CloseBox

    pure $ panel [ H.text "Show subjects with Order1 or concepts with Order2 ?"
                 , formChoiceSafe { items: [Order1, Order2]
                                 , default: methodGraphMetric'
                                 , callback: \val -> T.write_ val methodGraphMetric
                                 , print: show } []

{-
                 , H.text "NodeType 1 ?"
                 , formChoiceSafe { items: [GT.CTabTerms, GT.CTabSources, GT.CTabAuthors, GT.CTabInstitutes]
                                 , default: methodGraphNodeType1'
                                 , callback: \val -> T.write_ val methodGraphNodeType1
                                 , print: show } []

                 , H.text "NodeType 2 ?"
                 , formChoiceSafe { items: [GT.CTabTerms, GT.CTabSources, GT.CTabAuthors, GT.CTabInstitutes]
                                 , default: methodGraphNodeType2'
                                 , callback: \val -> T.write_ val methodGraphNodeType2
                                 , print: show } []
                                 -}
                 , H.text "Show Strong (expected) links or weak (maybe unexpected) links?"
                 , formChoiceSafe { items: [Strong, Weak]
                                 , default: methodGraphEdgesStrength'
                                 , callback: \val -> T.write_ val methodGraphEdgesStrength
                                 , print: show } []

{-
                 , formChoiceSafe { items: [Spinglass, Infomap, Confluence]
                                 , default: methodGraphClustering'
                                 , callback: \val -> T.write_ val methodGraphClustering
                                 , print: show } []
-}
                 ]
                 (submitButton (UpdateNode $ UpdateNodeParamsGraph { methodGraphMetric: methodGraphMetric'
                                                                   , methodGraphClustering: methodGraphClustering'
                                                                   , methodGraphEdgesStrength : methodGraphEdgesStrength'
                                                                   , methodGraphNodeType1 : methodGraphNodeType1'
                                                                   , methodGraphNodeType2 : methodGraphNodeType2'
                                                                   }
                               ) callback
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
        , exportFilter: 3.0
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
          launchAff_ do
            dispatch opts
            dispatch CloseBox

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
