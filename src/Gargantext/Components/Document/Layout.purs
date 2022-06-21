module Gargantext.Components.Document.Layout
  ( layout
  ) where

import Gargantext.Prelude

import Data.Maybe (Maybe(..), isJust, maybe)
import Data.String as String
import Data.Tuple.Nested ((/\))
import Gargantext.Components.Annotation.Field as AnnotatedField
import Gargantext.Components.AutoUpdate (autoUpdate)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (SpinnerTheme(..))
import Gargantext.Components.Document.Types (DocPath, Document(..), LoadedData, initialState)
import Gargantext.Core.NgramsTable.Functions (addNewNgramA, applyNgramsPatches, coreDispatch, findNgramRoot, setTermListA)
import Gargantext.Components.NgramsTable.AutoSync (useAutoSync)
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Core.NgramsTable.Types (CoreAction(..), Versioned(..), replace)
import Gargantext.Utils as U
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H

type Props =
  ( loaded   :: LoadedData
  , path     :: DocPath
  )

here :: R2.Here
here = R2.here "Gargantext.Components.Document.layout"

layout :: R2.Leaf Props
layout = R2.leaf layoutCpt

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
      } _ = do
    -- | States
    -- |

    state'@{ ngramsLocalPatch } /\ state <-
      R2.useBox' $ initialState { loaded }

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
        }

      setTermListOrAddA ngram Nothing        =
        addNewNgramA ngram
      setTermListOrAddA ngram (Just oldList) =
        setTermListA ngram <<< replace oldList

      setTermList ngram mOldList =
        dispatch <<< setTermListOrAddA (findNgramRoot ngrams ngram) mOldList

      hasAbstract =  maybe false (not String.null) doc.abstract

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
        H.div
        { className: "document-layout__controls" }
        [
          R2.when withAutoUpdate $

          --   -- (?) purpose? would still working with current code?
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

        ,
          R2.when' onPending'
          [
            B.cloak
            { isDisplayed: true
            , idlingPhaseDuration: Just 400
            , cloakSlot: (mempty :: R.Element)
            , defaultSlot:
                B.span'
                { className: "document-layout__controls__hint" }
                "currently saving"
            }
          ,
            B.spinner
            { theme: GrowTheme
            , className: "document-layout__controls__spinner"
            }
          ]
        ,
          R2.when (not onPending' && isJust result') $

            B.icon
            { name: "check-circle-o"
            , className: "document-layout__controls__icon"
            }
        ]
      ,
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


-------------------------------------------------------------

publicationDate :: Document -> String
publicationDate (Document {publication_year: Nothing}) = ""
publicationDate (Document {publication_year: Just py, publication_month: Nothing}) = U.zeroPad 2 py
publicationDate (Document {publication_year: Just py, publication_month: Just pm, publication_day: Nothing}) = (U.zeroPad 2 py) <> "-" <> (U.zeroPad 2 pm)
publicationDate (Document {publication_year: Just py, publication_month: Just pm, publication_day: Just pd}) = (U.zeroPad 2 py) <> "-" <> (U.zeroPad 2 pm) <> "-" <> (U.zeroPad 2 pd)
