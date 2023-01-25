module Gargantext.Components.Forest.Tree.Node.Action.WriteNodesDocuments where

import Gargantext.Prelude

import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Gargantext.Components.App.Store (Boxes)
import Gargantext.Components.Forest.Tree.Node.Action.Types (Action(..))
import Gargantext.Components.Forest.Tree.Node.Tools (panel, submitButton)
import Gargantext.Config.REST (AffRESTError, RESTError)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, post)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Action.WriteNodesDocuments"

-- | Action : WriteNodesDocuments
type ActionWriteNodesDocuments =
  ( boxes    :: Boxes
  , dispatch :: Action -> Aff Unit
  , id       :: GT.ID
  , session  :: Session )

actionWriteNodesDocuments :: R2.Component ActionWriteNodesDocuments
actionWriteNodesDocuments = R.createElement actionWriteNodesDocumentsCpt
actionWriteNodesDocumentsCpt :: R.Component ActionWriteNodesDocuments
actionWriteNodesDocumentsCpt = here.component "actionWriteNodesDocuments" cpt where
  cpt { boxes, dispatch, id, session } _ = do
    let bodies =
          [ R2.row
            [ H.div { className: "col-12 flex-space-around" }
              [ H.div { className: "form-group" }
                [ H.text "Will traverse all Write Nodes and insert them as documents into current corpus." ]
              ]
            ]
          ]

    pure $ panel bodies (submitButton (DocumentsFromWriteNodes { id }) dispatch)


documentsFromWriteNodesReq :: Session -> GT.ID -> AffRESTError GT.AsyncTaskWithType
documentsFromWriteNodesReq session id = do
  eTask :: Either RESTError GT.AsyncTask <-
    post session (NodeAPI GT.Node (Just id) "documents-from-write-nodes") { id }
  pure $ (\task -> GT.AsyncTaskWithType { task, typ: GT.UpdateNode }) <$> eTask
