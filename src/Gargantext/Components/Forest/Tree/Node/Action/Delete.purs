module Gargantext.Components.Forest.Tree.Node.Action.Delete
  where

import Gargantext.Prelude

import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Components.Forest.Tree.Node.Action (Action(..))
import Gargantext.Components.Forest.Tree.Node.Tools (submitButton, panel)
import Gargantext.Config.REST (RESTError)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, delete, put_)
import Gargantext.Types (NodeType(..))
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Action.Delete"

-- TODO Delete with asyncTaskWithType

deleteNode :: Session -> NodeType -> GT.ID -> Aff (Either RESTError GT.ID)
deleteNode session nt nodeId = delete session $ NodeAPI GT.Node (Just nodeId) ""

{-
  case nt of
    NodePublic FolderPublic -> delete session $ NodeAPI GT.Node (Just nodeId) ""
    NodePublic _ -> put_   session $ NodeAPI GT.Node (Just nodeId) "unpublish"
    _            -> delete session $ NodeAPI GT.Node (Just nodeId) ""
    -}

type ParentID = GT.ID
unpublishNode :: Session -> Maybe ParentID -> GT.ID -> Aff (Either RESTError GT.ID)
unpublishNode s p n = put_ s $ NodeAPI GT.Node p ("unpublish/" <> show n)


-- | Action : Delete
type Delete =
  ( dispatch :: Action -> Aff Unit
  , nodeType :: NodeType )

actionDelete :: R2.Component Delete
actionDelete = R.createElement actionDeleteCpt
actionDeleteCpt :: R.Component Delete
actionDeleteCpt = here.component "actionDelete" cpt where
  cpt props@{ nodeType: NodeUser } _ = pure $ actionDeleteUser props []
  cpt props                        _ = pure $ actionDeleteOther props []

actionDeleteUser :: R2.Component Delete
actionDeleteUser = R.createElement actionDeleteUserCpt
actionDeleteUserCpt :: R.Component Delete
actionDeleteUserCpt = here.component "actionDeleteUser" cpt where
  cpt _  _ = do
    pure $ panel [ H.div { style: {margin: "10px"}}
                    [ H.text $ "Yes, we are RGPD compliant!"
                    <> " But you can not delete User Node yet."
                    <> " We are still on development."
                    <> " Thanks for your comprehensin."
                    ]
            ] (H.div {} [])

actionDeleteOther :: R2.Component Delete
actionDeleteOther = R.createElement actionDeleteOtherCpt
actionDeleteOtherCpt :: R.Component Delete
actionDeleteOtherCpt = here.component "actionDeleteOther" cpt where
  cpt { dispatch, nodeType } _ = do
    pure $ panel (map (\t -> H.p {} [H.text t])
                       [ "Are your sure you want to delete it ?"
                       , "If yes, click again below."
                       ]
                  ) (submitButton (DeleteNode nodeType) dispatch)
