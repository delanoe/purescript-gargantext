module Gargantext.Components.Forest.Tree.Node.Action.Update where

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Prelude

import Gargantext.Components.Forest.Tree.Node.Action.Types (Action(..))
import Gargantext.Components.Forest.Tree.Node.Action.Update.Types
import Gargantext.Components.Forest.Tree.Node.Tools (formChoiceSafe, submitButton, panel)
import Gargantext.Config.REST (RESTError)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, post)
import Gargantext.Types (NodeType(..), ID)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Action.Update"

updateRequest :: UpdateNodeParams -> Session -> ID -> Aff (Either RESTError GT.AsyncTaskWithType)
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
  cpt props@{ nodeType: _         } _ = pure $ updateOther props []

updateDashboard :: R2.Component UpdateProps
updateDashboard = R.createElement updateDashboardCpt
updateDashboardCpt :: R.Component UpdateProps
updateDashboardCpt = here.component "updateDashboard" cpt where
  cpt { dispatch } _ = do
    methodBoard <- T.useBox All
    methodBoard' <- T.useLive T.unequal methodBoard

    pure $ panel [ -- H.text "Update with"
                  formChoiceSafe [All, Sources, Authors, Institutes, Ngrams] All (\val -> T.write_ val methodBoard) show
                 ]
                 (submitButton (UpdateNode $ UpdateNodeParamsBoard { methodBoard: methodBoard' }) dispatch)

updateGraph :: R2.Component UpdateProps
updateGraph = R.createElement updateGraphCpt
updateGraphCpt :: R.Component UpdateProps
updateGraphCpt = here.component "updateGraph" cpt where
  cpt { dispatch } _ = do
    methodGraph <- T.useBox Order1
    methodGraph' <- T.useLive T.unequal methodGraph

    pure $ panel [ -- H.text "Update with"
                  formChoiceSafe [Order1, Order2] Order1 (\val -> T.write_ val methodGraph) show
                 ]
                 (submitButton (UpdateNode $ UpdateNodeParamsGraph { methodGraph: methodGraph' }) dispatch)

updateNodeList :: R2.Component UpdateProps
updateNodeList = R.createElement updateNodeListCpt
updateNodeListCpt :: R.Component UpdateProps
updateNodeListCpt = here.component "updateNodeList" cpt where
  cpt { dispatch } _ = do
    methodList <- T.useBox Basic
    methodList' <- T.useLive T.unequal methodList

    pure $ panel [ -- H.text "Update with"
                  formChoiceSafe [Basic, Advanced, WithModel] Basic (\val -> T.write_ val methodList) show
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
                  formChoiceSafe [NewNgrams, NewTexts, Both] NewNgrams (\val -> T.write_ val methodTexts) show
                 ]
                 (submitButton (UpdateNode $ UpdateNodeParamsTexts { methodTexts: methodTexts' }) dispatch)

updateOther :: R2.Component UpdateProps
updateOther = R.createElement updateOtherCpt
updateOtherCpt :: R.Component UpdateProps
updateOtherCpt = here.component "updateOther" cpt where
  cpt _ _ = do
    pure $ H.div {} []

-- fragmentPT $ "Update " <> show nodeType
