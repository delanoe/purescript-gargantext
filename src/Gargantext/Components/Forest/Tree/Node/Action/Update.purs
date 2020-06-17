module Gargantext.Components.Forest.Tree.Node.Action.Update where

import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Gargantext.Components.Forest.Tree.Node.Action (Action(..))
import Gargantext.Components.Forest.Tree.Node.Action.Update.Types
import Gargantext.Components.Forest.Tree.Node.Tools (formChoiceSafe, submitButton, panel)
import Gargantext.Types (NodeType(..), ID)
import Gargantext.Types  as GT
import Gargantext.Prelude (Unit, bind, ($), pure)
import Gargantext.Sessions (Session, put)
import Gargantext.Routes as GR
import Reactix as R
import Reactix.DOM.HTML as H


updateRequest :: UpdateNodeParams -> Session -> ID -> Aff GT.AsyncTaskWithType
updateRequest (UpdateNodeParamsList meth) session nodeId = do
  task <- put session p meth
  pure $ GT.AsyncTaskWithType {task, typ: GT.UpdateNode } -- TODO add NodeType
    where
      p = GR.NodeAPI GT.Node (Just nodeId) (GT.asyncTaskTypePath GT.UpdateNode)

updateRequest (UpdateNodeParamsGraph meth) session nodeId = do
  task <- put session p meth
  pure $ GT.AsyncTaskWithType {task, typ: GT.UpdateNode } -- TODO add NodeType
    where
      p = GR.NodeAPI GT.Node (Just nodeId) (GT.asyncTaskTypePath GT.UpdateNode)

updateRequest (UpdateNodeParamsTexts meth) session nodeId = do
  task <- put session p meth
  pure $ GT.AsyncTaskWithType {task, typ: GT.UpdateNode } -- TODO add NodeType
    where
      p = GR.NodeAPI GT.Node (Just nodeId) (GT.asyncTaskTypePath GT.UpdateNode)

----------------------------------------------------------------------

update ::  NodeType
       -> (Action -> Aff Unit)
       ->  R.Hooks R.Element
update NodeList dispatch = do
  meth @( method /\ setMethod  ) <- R.useState' Basic
  pure $ panel [ -- H.text "Update with"
                formChoiceSafe [Basic, Advanced, WithModel] Basic setMethod
               ]
               (submitButton (UpdateNode $ UpdateNodeParamsList {method}) dispatch)

update Graph dispatch = do
  meth @( method /\ setMethod  ) <- R.useState' Order1
  pure $ panel [ -- H.text "Update with"
                formChoiceSafe [Order1, Order2] Order1 setMethod
               ]
               (submitButton (UpdateNode $ UpdateNodeParamsGraph {method}) dispatch)

update Texts dispatch  = do
  meth @( method /\ setMethod  ) <- R.useState' NewNgrams
  pure $ panel [ -- H.text "Update with"
                formChoiceSafe [NewNgrams, NewTexts, Both] NewNgrams setMethod
               ]
               (submitButton (UpdateNode $ UpdateNodeParamsTexts {method}) dispatch)

update _     _  = pure $ H.div {} []

-- fragmentPT $ "Update " <> show nodeType
