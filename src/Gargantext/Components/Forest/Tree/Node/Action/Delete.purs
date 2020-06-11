module Gargantext.Components.Forest.Tree.Node.Action.Delete
  where

import Data.Maybe (Maybe(..))
import Gargantext.Prelude
import Effect.Aff (Aff)
import Gargantext.Types  as GT
import Gargantext.Sessions (Session, delete)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Types (NodeType(..))
import Gargantext.Components.Forest.Tree.Node.Action (Action(..))
import Reactix as R
import Gargantext.Components.Forest.Tree.Node.Tools (submitButton)
import Reactix.DOM.HTML as H

-- TODO Delete with asyncTaskWithType
deleteNode :: Session -> GT.ID -> Aff GT.ID
deleteNode session nodeId = delete session $ NodeAPI GT.Node (Just nodeId) ""

-- | Action : Delete
actionDelete :: NodeType -> (Action -> Aff Unit) -> R.Hooks R.Element
actionDelete NodeUser _ = do
  pure $ R.fragment [
    H.div {style: {margin: "10px"}} 
          [H.text $ "Yes, we are RGPD compliant!"
                 <> " But you can not delete User Node yet."
                 <> " We are still on development."
                 <> " Thanks for your comprehensin."
          ]
  ]

actionDelete _ dispatch = do
  pure $ R.fragment [
    H.div {style: {margin: "10px"}} 
          (map (\t -> H.p {} [H.text t]) 
               [ "Are your sure you want to delete it ?"
               , "If yes, click again below."
               ]
          )
    , submitButton DeleteNode dispatch
    ]



