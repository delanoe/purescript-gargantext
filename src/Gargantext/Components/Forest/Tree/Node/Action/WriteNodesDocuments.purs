module Gargantext.Components.Forest.Tree.Node.Action.WriteNodesDocuments where

import Gargantext.Prelude

import Data.Array as A
import Data.Either (Either)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Gargantext.Components.App.Store (Boxes)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Forest.Tree.Node.Action.Types (Action(..))
import Gargantext.Components.Forest.Tree.Node.Action.Utils (loadLanguages)
import Gargantext.Components.Forest.Tree.Node.Tools (panel, submitButton)
import Gargantext.Components.Lang (Lang(..))
import Gargantext.Components.ListSelection as ListSelection
import Gargantext.Components.ListSelection.Types as ListSelection
import Gargantext.Config.REST (AffRESTError, RESTError(..))
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, post)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Toestand as T


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
  cpt props@({ session }) _ = do
    useLoader { errorHandler
              , loader: loadLanguages
              , path: { session }
              , render: \langs ->
              actionWriteNodesDocumentsWithLangs (Record.merge props { langs }) [] }
    where
      errorHandler err = case err of
        ReadJSONError err' -> here.warn2 "[actionSearch] ReadJSONError" $ show err'
        _                  -> here.warn2 "[actionSearch] RESTError" err

type ActionWriteNodesDocumentsWithLangs =
  ( langs :: Array Lang
  | ActionWriteNodesDocuments )

actionWriteNodesDocumentsWithLangs :: R2.Component ActionWriteNodesDocumentsWithLangs
actionWriteNodesDocumentsWithLangs = R.createElement actionWriteNodesDocumentsWithLangsCpt
actionWriteNodesDocumentsWithLangsCpt :: R.Component ActionWriteNodesDocumentsWithLangs
actionWriteNodesDocumentsWithLangsCpt = here.component "actionWriteNodesDocumentsWithLangs" cpt where
  cpt { dispatch, id, langs, session } _ = do

    lang' /\ langBox
        <- R2.useBox' $ fromMaybe Universal $ A.head langs
    selection' /\ selectionBox
        <- R2.useBox' ListSelection.MyListsFirst
    paragraphs' /\ paragraphBox
        <- R2.useBox' "7"

    let bodies = [
      H.div
      { className: "col-12 flex-space-around" }
      [ H.h4 {}
        [ H.text "Will traverse all Write Nodes and insert them as documents into current corpus." ]
      ]
    ,
      -- lang
      H.div
      { className: "form-group" }
      [
        H.div
        { className: "form-group__label" }
        [
          B.label_ $
          "File lang"
        ]
      ,
        H.div
        { className: "form-group__field" }
        [
          B.formSelect'
          { callback: flip T.write_ langBox
          , value: lang'
          , list: langs <> [ No_extraction, Universal ]
          }
          []
        ]
      ]
    ,
      -- paragraph
      H.div
      { className: "form-group "}
      [
        H.div
        { className: "form-group__label" }
        [
          B.label_ $
          "Paragraph size (sentences)"
        ]
      ,
        H.div
        { className: "form-group__field" }
        [
          B.formInput
          { callback: flip T.write_ paragraphBox
          , value: paragraphs'
          }
        ]
      ]
    ,
      --selection
      H.div
      { className: "form-group" }
      [
        H.div
        { className: "form-group__label" }
        [
          B.label_ $
            "List selection"
        ]
      ,
        H.div
        { className: "form-group__field" }
        [
          ListSelection.selection
          { selection: selectionBox
          , session
          } []
        ]
      ]
    ]

    pure $ panel bodies (submitButton (DocumentsFromWriteNodes { id, lang: lang', selection: selection', paragraphs: paragraphs' }) dispatch)


type Params =
  ( id         :: GT.ID
  , selection  :: ListSelection.Selection
  , lang       :: Lang
  , paragraphs :: String
  )

documentsFromWriteNodesReq :: Session -> Record Params -> AffRESTError GT.AsyncTaskWithType
documentsFromWriteNodesReq session params@{ id } = do
  eTask :: Either RESTError GT.AsyncTask <-
    post session (NodeAPI GT.Node (Just id) "documents-from-write-nodes") params
  pure $ (\task -> GT.AsyncTaskWithType { task, typ: GT.UpdateNode }) <$> eTask
