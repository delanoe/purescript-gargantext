module Gargantext.Components.DocsTable.DocumentFormCreation
  ( documentFormCreation
  , FormData
  , create, createProgress
  ) where

import Gargantext.Prelude

import DOM.Simple.Console (log3)
import Data.Either (Either(..))
import Data.Foldable (foldl, intercalate)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), ComponentStatus(..), Variant(..))
import Gargantext.Config.REST (AffRESTError)
import Gargantext.Hooks.FormValidation (VForm, useFormValidation)
import Gargantext.Hooks.FormValidation.Unboxed as FV
import Gargantext.Hooks.StateRecord (useStateRecord)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, post, get)
import Gargantext.Types as GT
import Gargantext.Utils (nbsp, (?))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Record.Extra (pick)
import Type.Proxy (Proxy(..))

type Props =
  ( callback  :: Record FormData -> Effect Unit
  , status    :: ComponentStatus
  | Options
  )

type Options = ( | FormData )

options :: Record Options
options = Record.merge {} defaultData

documentFormCreation :: forall r. R2.OptLeaf Options Props r
documentFormCreation = R2.optLeaf component options

component :: R.Component Props
component = R.hooksComponent "documentFormCreation" cpt where
  cpt props _ = do
    -- Hooks
    { state, bindStateKey } <- useStateRecord (pick props :: Record FormData)
    fv <- useFormValidation

    -- @onSubmit: exec whole form validation and execute callback
    onSubmit <- pure $ do

      result <- fv.try (\_ -> documentFormValidation state)

      case result of
        Left err -> log3 "documentFormCreation validation error" state err
        Right _  -> props.callback state

    -- Render
    pure $

      H.form
      { className: "document-form-creation" }
      [
        -- Title
        H.div
        { className: intercalate " "
            [ "form-group"
            , (fv.hasError' "title") ?
                "form-group--error" $
                mempty
            ]
        }
        [
          H.div { className: "form-group__label" }
          [
            H.label {} [ H.text "Title" ]
          ]
        ,
          H.div { className: "form-group__field" }
          [
            B.formInput $
              bindStateKey "title"
          ,
            R2.when (fv.hasError' "title") $
              H.div { className: "form-group__error" }
              [ H.text "Please enter a title" ]
          ]
        ]
      ,
        -- Source
        H.div
        { className: intercalate " "
            [ "form-group"
            , (fv.hasError' "source") ?
                "form-group--error" $
                mempty
            ]
        }
        [
          H.div { className: "form-group__label" }
          [
            H.label {} [ H.text "Source" ]
          ]
        ,
          H.div { className: "form-group__field" }
          [
            B.formInput $
              bindStateKey "source"
          ,
            R2.when (fv.hasError' "source") $
              H.div { className: "form-group__error" }
              [ H.text "Please enter a source" ]
          ]
        ]
      ,
        -- Authors
        H.div
        { className: intercalate " "
            [ "form-group"
            , (fv.hasError' "authors") ?
                "form-group--error" $
                mempty
            ]
        }
        [
          H.div { className: "form-group__label" }
          [
            H.label {} [ H.text "Authors" ]
          ]
        ,
          H.div { className: "form-group__field" }
          [
            B.formInput $
            { placeholder: "ex: author1, author2, …"
            } `Record.merge` bindStateKey "authors"
          ,
            R2.when (fv.hasError' "authors") $
              H.div { className: "form-group__error" }
              [ H.text "Please enter at least one author" ]
          ]
        ]
      ,
        -- Date
        H.div
        { className: intercalate " "
            [ "form-group"
            , (fv.hasError' "date") ?
                "form-group--error" $
                mempty
            ]
        }
        [
          H.div
          { className: "form-group__label" }
          [
            H.label {} [ H.text $ "Date" ]
          ]
        ,
          H.div
          { className: "form-group__field" }
          [
            B.formInput $
            { type: "date"
            } `Record.merge` bindStateKey "date"
          ,
            R2.when (fv.hasError' "date") $
              H.div { className: "form-group__error" }
              [ H.text "Please enter a valid date" ]
          ]
        ]
      ,
        -- Abstract
        H.div
        { className: intercalate " "
            [ "form-group"
            ]
        }
        [
          H.div
          { className: "form-group__label" }
          [
            H.label {} [ H.text $ "Abstract" <> nbsp 1 ]
          ,
            H.span
            { className: "form-group__label--sub" }
            [ H.text "optional" ]
          ]
        ,
          H.div
          { className: "form-group__field" }
          [
            B.formTextarea $
            { rows: 5
            } `Record.merge` bindStateKey "abstract"
          ]
        ]
      ,
        -- Submit
        H.div { className: "document-form-creation__submit" }
        [
          B.button
          { callback: \_ -> onSubmit
          , status: props.status == Deferred ? Deferred $ Enabled
          , variant: ButtonVariant Primary
          , type: "submit"
          , block: true
          }
          [ H.text "Add" ]
        ]
      ]

type FormData =
  ( title     :: String
  , source    :: String
  , authors   :: String
  , date      :: String
  , abstract  :: String
  )

defaultData :: Record FormData
defaultData =
  { title     : ""
  , source    : ""
  , authors   : ""
  , date      : ""
  , abstract  : ""
  }

documentFormValidation :: Record FormData -> Effect VForm
documentFormValidation r = foldl append mempty rules
  where
    rules =
      [ FV.nonEmpty "title" r.title
      , FV.nonEmpty "source" r.source
      , FV.nonEmpty "authors" r.authors
      , FV.date "date" r.date
      ]


---------------------------------------------------

create ::
     Session
  -> GT.ID
  -> Record FormData
  -> AffRESTError GT.AsyncTaskWithType
create session nodeId =
      rename
  >>> post session request
  >=> case _ of
    Left err   -> pure $ Left err
    Right task -> pure $ Right $ GT.AsyncTaskWithType
      { task
      , typ: GT.NodeDocument
      }

  where

    request = GR.NodeAPI GT.Node (Just nodeId)
      (GT.asyncTaskTypePath GT.NodeDocument)

    rename = Record.rename
      (Proxy :: Proxy "source")
      (Proxy :: Proxy "sources")

createProgress ::
    Session
  -> GT.ID
  -> GT.AsyncTaskWithType
  -> AffRESTError GT.AsyncProgress
createProgress
  session
  nodeId
  (GT.AsyncTaskWithType { task: GT.AsyncTask { id } })
  =
    get session request

  where

    request = GR.NodeAPI GT.Node (Just nodeId)
      (GT.asyncTaskTypePath GT.NodeDocument <> pollParams)

    pollParams = "/" <> id <> "/poll?limit1"
