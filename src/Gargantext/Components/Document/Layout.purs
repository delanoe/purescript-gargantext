module Gargantext.Components.Document.Layout
  ( layout
  ) where

import Gargantext.Prelude

import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String as String
import Data.Tuple.Nested ((/\))
import Gargantext.Components.Annotation.Field as AnnotatedField
import Gargantext.Components.Annotation.Types as AFT
import Gargantext.Components.AutoUpdate (autoUpdate)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (SpinnerTheme(..))
import Gargantext.Components.Document.Types (DocPath, Document(..), LoadedData, initialState)
import Gargantext.Components.NgramsTable.AutoSync (useAutoSync)
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Core.NgramsTable.Functions (addNewNgramA, applyNgramsPatches, coreDispatch, findNgramRoot, setTermListA)
import Gargantext.Core.NgramsTable.Types (CoreAction(..), Versioned(..), replace)
import Gargantext.Utils as U
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

type Props =
  ( loaded   :: LoadedData
  , path     :: DocPath
  | Options
  )

type Options =
  ( sideControlsSlot :: Maybe R.Element
  )

options :: Record Options
options =
  { sideControlsSlot: Nothing
  }

here :: R2.Here
here = R2.here "Gargantext.Components.Document.layout"

layout :: forall r. R2.OptLeaf Options Props r
layout = R2.optLeaf layoutCpt options

layoutCpt :: R.Component Props
layoutCpt = here.component "main" cpt where
  -- Component
  cpt { path
      , loaded:
          loaded@{ ngramsTable: Versioned
          { data: initTable }
          , document: NodePoly
            { hyperdata: Document doc
            }
          }
      , sideControlsSlot
      } _ = do
    -- | States
    -- |

    state'@{ ngramsLocalPatch } /\ state <-
      R2.useBox' $ initialState { loaded }

    mode' /\ mode <- R2.useBox' AFT.EditionMode

    -- | Hooks
    -- |
    let dispatch = coreDispatch path state

    { onPending, result } <- useAutoSync { state, action: dispatch }

    onPending' <- R2.useLive' onPending
    result'    <- R2.useLive' result

    -- | Computed
    -- |
    let
      withAutoUpdate = false

      ngrams = applyNgramsPatches state' initTable

      annotate text = AnnotatedField.annotatedField
        { ngrams
        , setTermList
        , text
        , mode: mode'
        }

      setTermListOrAddA ngram Nothing        =
        addNewNgramA ngram
      setTermListOrAddA ngram (Just oldList) =
        setTermListA ngram <<< replace oldList

      setTermList ngram mOldList =
        dispatch <<< setTermListOrAddA (findNgramRoot ngrams ngram) mOldList

      hasAbstract =  maybe false (not String.null) doc.abstract

    -- | Behaviors
    -- |
    let
      onModeChange = read >>> fromMaybe AFT.EditionMode >>> flip T.write_ mode

    -- | Render
    -- |
    pure $

      H.div
      { className: "document-layout" }
      --DEBUG
      --[ H.pre { rows: 30 } [
      --    H.text (stringifyWithIndent 2 (encodeJson (fst state)))
      --  ] ] <>
      [
        -- Header
        H.div
        { className: "document-layout__header" }
        [
          H.div
          { className: "document-layout__main-controls" }
          [
            -- Viewing mode
            B.wad
            [ "d-flex", "align-items-center" ]
            [
              H.label
              { className: "mr-1"
              }
              [
                B.icon
                { name: "tags" }
              ]
            ,
              B.formSelect
              { value: show mode'
              , callback: onModeChange
              }
              [
                H.option
                { value: show AFT.AdditionMode }
                [ H.text "Add terms only" ]
              ,
                H.option
                { value: show AFT.EditionMode }
                [ H.text "Add and edit terms" ]
              ]
            ]
          ,
            R2.when withAutoUpdate $
              -- (?) purpose? would still working with current code?
              autoUpdate
              { duration: 5000
              , effect: dispatch $ Synchronize
                { afterSync: \_ -> pure unit
                }
              }
          -- @NOTE #386: revert manual for automatic sync
          --   syncResetButtons
          --   { afterSync
          --   , ngramsLocalPatch
          --   , performAction: dispatch
          --   }
          ]
        ,
          H.div
          { className: "document-layout__side-controls" }
          [
            -- Saving informations
            H.div
            { className: "document-layout__saving" }
            [
              R2.when' onPending'
              [
                B.spinner
                { theme: GrowTheme
                , className: "document-layout__saving__spinner"
                }
              ]
            ,
              R2.when (not onPending' && isJust result') $

                B.icon
                { name: "check"
                , className: "document-layout__saving__icon"
                }
            ]
          ,
            R2.fromMaybe sideControlsSlot identity
          ]
        ]
      ,
        -- Body
        H.div
        { className: "document-layout__body" }
        [
          B.div'
          { className: "document-layout__separator-label" }
          "Title"
        ,
          H.div
          { className: "document-layout__title" }
          [
            annotate doc.title
          ]
        ,
          R2.fromMaybe doc.authors \authors ->

            H.div
            { className: "document-layout__authors" }
            [
              B.div'
              { className: "document-layout__authors__label" }
              "Authors"
            ,
              H.div
              { className: "document-layout__authors__content" }
              [
                -- @NOTE #386: annotate for "Authors" ngrams list
                annotate (Just authors)
              ]
            ]
        ,
          R2.fromMaybe doc.source \source ->

            H.div
            { className: "document-layout__source" }
            [
              B.div'
              { className: "document-layout__source__label" }
              "Source"
            ,
              B.div'
              { className: "document-layout__source__content" }
              source
            ]
        ,
            H.div
            { className: "document-layout__date" }
            [
              B.div'
              { className: "document-layout__date__label" }
              "Date"
            ,
              B.div'
              { className: "document-layout__date__content" }
              (publicationDate $ Document doc)
            ]
        ,
          R2.when hasAbstract $

            H.div
            { className: "document-layout__abstract" }
            [
              B.div'
              { className: "document-layout__separator-label" }
              "Abstract"
            ,
              H.div
              { className: "document-layout__abstract__content" }
              [
                annotate doc.abstract
              ]
            ]
        -- (?) remove "Full text" block (unused feature for now,
        --     see #334)
        -- , H.div { className: "jumbotron" } [ H.p {} [ H.text "Empty Full Text" ] ]
        ]
      ]


-------------------------------------------------------------

publicationDate :: Document -> String
publicationDate (Document {publication_year: Nothing}) = ""
publicationDate (Document {publication_year: Just py, publication_month: Nothing}) = U.zeroPad 2 py
publicationDate (Document {publication_year: Just py, publication_month: Just pm, publication_day: Nothing}) = (U.zeroPad 2 py) <> "-" <> (U.zeroPad 2 pm)
publicationDate (Document {publication_year: Just py, publication_month: Just pm, publication_day: Just pd}) = (U.zeroPad 2 py) <> "-" <> (U.zeroPad 2 pm) <> "-" <> (U.zeroPad 2 pd)
