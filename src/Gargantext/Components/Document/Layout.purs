module Gargantext.Components.Document.Layout
  ( layout
  ) where

import Gargantext.Prelude

import Data.Array as A
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Set as Set
import Data.String as String
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Gargantext.Components.Annotation.Field as AnnotatedField
import Gargantext.Components.Annotation.Types as AFT
import Gargantext.Components.AutoUpdate (autoUpdate)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ComponentStatus(..), SpinnerTheme(..))
import Gargantext.Components.Category (ratingSimpleLoader)
import Gargantext.Components.Document.Types (DocPath, Document(..), LoadedData, initialState)
import Gargantext.Components.GraphQL.Endpoints (getContextNgrams)
import Gargantext.Components.NgramsTable.AutoSync (useAutoSync)
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Core.NgramsTable.Functions (addNewNgramA, applyNgramsPatches, coreDispatch, findNgramRoot, setTermListA, computeCache)
import Gargantext.Core.NgramsTable.Types (CoreAction(..), NgramsTerm, Versioned(..), replace)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Sessions (Session)
import Gargantext.Utils as U
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Toestand as T

-------------------------------------------------------------------------


here :: R2.Here
here = R2.here "Gargantext.Components.Document.Layout"


type Props =
  ( loaded   :: LoadedData
  , path     :: DocPath
  , session  :: Session
  | Options
  )

type Options =
  ( sideControlsSlot :: Maybe R.Element
  )

options :: Record Options
options =
  { sideControlsSlot: Nothing
  }

layout :: forall r. R2.OptLeaf Options Props r
layout = R2.optLeaf layoutCpt options
layoutCpt :: R.Component Props
layoutCpt = R2.hereComponent here "layout" hCpt where
  hCpt hp props@{ path: { listIds
                        , nodeId }
                , session } _ = do
    case A.head listIds of
      Nothing -> pure $ H.div {} [ H.text "No list supplied!" ]
      Just listId ->
        useLoader { errorHandler: Nothing
                  , herePrefix: hp
                  , loader: \p -> getContextNgrams session p.contextId p.listId
                  , path: { contextId: nodeId, listId }
                  , render: \contextNgrams ->
                      layoutWithContextNgrams $ Record.merge props { contextNgrams } }

type WithContextNgramsProps =
  ( contextNgrams :: Array NgramsTerm
  | Props )

layoutWithContextNgrams :: forall r. R2.OptLeaf Options WithContextNgramsProps r
layoutWithContextNgrams = R2.optLeaf layoutWithContextNgramsCpt options
layoutWithContextNgramsCpt :: R.Component WithContextNgramsProps
layoutWithContextNgramsCpt = here.component "layoutWithContextNgrams" cpt where
  -- Component
  cpt { contextNgrams
      , loaded:
          loaded@{ ngramsTable: Versioned { data: initTable }
                 , document: NodePoly { hyperdata: Document doc }
                 }
      , path
      , sideControlsSlot
      } _ = do
    -- | States
    -- |

    contextNgramsS <- T.useBox contextNgrams
    contextNgrams' <- T.useLive T.unequal contextNgramsS

    -- ngrams <- T.useBox initTable
    -- ngrams' <- T.useLive T.unequal ngrams
    state <- T.useBox $ initialState { loaded }
    state' <- T.useLive T.unequal state
    let ngrams' = applyNgramsPatches state' initTable

    -- R.useEffect' $ do
    --   let (NgramsTable { ngrams_repo_elements: nre }) = ngrams'
    --   let nre' = Map.mapMaybeWithKey (\k v -> Just { k, v }) nre
    --   here.log2 "[layout] nre" $ A.fromFoldable nre'
    --   here.log2 "[layout] contextNgrams" contextNgrams
    --   here.log2 "[layout] contextNgrams'" contextNgrams'
    --   here.log2 "[layout] state'" state'
    --   here.log2 "[layout] ngrams'" ngrams'

    mode' /\ mode <- R2.useBox' AFT.EditionMode

    let dispatch = coreDispatch path state
    { onPending, result } <- useAutoSync { state, action: dispatch }

    onPending' <- R2.useLive' onPending
    result'    <- R2.useLive' result

    -- | Computed
    -- |
    let

      withAutoUpdate = false

      -- ngrams = applyNgramsPatches state' initTable

      cache = computeCache ngrams' $ Set.fromFoldable contextNgrams'

      setTermListOrAddA ngram Nothing        =
        addNewNgramA ngram
      setTermListOrAddA ngram (Just oldList) =
        setTermListA ngram <<< replace oldList

      setTermList ngram mOldList termList = do
        let root = findNgramRoot ngrams' ngram
        -- here.log2 "[setTermList] ngram" ngram
        -- here.log2 "[setTermList] root" root
        let patch = setTermListOrAddA root mOldList termList
        -- here.log2 "[setTermList] patch" patch
        dispatch patch
        -- T.write_ (applyNgramsPatches state' initTable) ngrams
        case mOldList of
          Nothing -> T.write_ (contextNgrams' <> [ root ]) contextNgramsS
          Just _ -> pure unit
        -- here.log2 "[setTermList] calling reload" reload'
        -- T2.reload reload

      hasAbstract =  maybe false (not String.null) doc.abstract

      annotate text = AnnotatedField.annotatedField
                      { ngrams'
                      , setTermList
                      , text
                      , mode: mode'
                      , cache
                      }

    -- | Hooks
    -- |

    -- R.useEffect' $ do
    --   let NgramsTable { ngrams_repo_elements } = ngrams
    --   here.log2 "[layout] length of ngrams" $ Map.size ngrams_repo_elements
    --   here.log2 "[layout] length of pats" $ A.length cache.pats
    --   here.log2 "[layout] contextNgrams" contextNgrams

    -- | Behaviors
    -- |
    let
      onModeChange = read >>> fromMaybe AFT.EditionMode >>> flip T.write_ mode

    -- R.useEffect' $ do
    --   here.log2 "[layoutWithContextNgrams] cache" cache
    --   here.log2 "[layoutWithContextNgrams] ngramsLocalPatch" ngramsLocalPatch
    --   let NgramsTable { ngrams_repo_elements: nre } = initTable
    --   here.log2 "[layoutWithContextNgrams] ngrams_repo_elements" $ A.fromFoldable $ Map.keys nre
    --   let NgramsTable { ngrams_repo_elements: nre' } = ngrams'
    --   here.log2 "[layoutWithContextNgrams] ngrams (after apply patches)" $ A.fromFoldable $ Map.keys nre'

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
            [ "d-flex", "align-items-center", "width-auto" ]
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
              , status: Enabled
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
                { afterSync: \_ -> do
                                   liftEffect $ here.log "[autoSync] synchronize war run"
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
          H.div
          { className: "document-layout__title d-flex" }
          [
            H.div
            { className: "document-layout__title__content" -- <> collapsibleClasses <> " variant-collapse-higher"
                , id: getIdName "title" }
            [
              annotate doc.title
            ]
          -- ,
          --   btnSeeMore "title"
          ]
        ,
          R2.fromMaybe doc.authors \authors ->

            H.div
            { className: "document-layout__authors justify-content-space-between" }
            [
              B.div'
              { className: "document-layout__authors__label" }
              "Authors"
            ,
              H.div
              { className: "document-layout__authors__content w-100" -- <> collapsibleClasses
              , id: getIdName "authors" }
              [
                -- @NOTE #386: annotate for "Authors" ngrams list
                annotate (Just authors)
              ]
            -- ,
            --   btnSeeMore "authors"
            ]
        ,
          R2.fromMaybe doc.source \source ->

            H.div
            { className: "document-layout__source justify-content-space-between" }
            [
              B.div'
              { className: "document-layout__source__label" }
              "Source"
            ,
              B.div'
              { className: "document-layout__source__content w-100" -- <> collapsibleClasses
              , id: getIdName "sources" }
              source
            -- ,
            --   btnSeeMore "sources"
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
        , case path.mCorpusId of
            Nothing -> H.div {} []
            Just corpusId -> ratingSimpleLoader { docId: path.nodeId
                                                , corpusId
                                                , session: path.session  } []
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


-- btnSeeMore :: String -> R.Element
-- btnSeeMore idStr =
--   H.a { role: "button"
--   , className: "collapsed btn-seemore text-primary"
--   , data: { toggle: "collapse" }
--   , aria: { expanded: false, controls: getIdName idStr }
--   , href: "#" <> getIdName idStr
--   }
--   [ H.text "" ]

getIdName :: String -> String
getIdName str = "annotated-field-expand__" <> str

-- collapsibleClasses :: String
-- collapsibleClasses = " annotated-field-expand collapse"
