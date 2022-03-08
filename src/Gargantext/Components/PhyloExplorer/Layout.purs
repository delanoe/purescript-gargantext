module Gargantext.Components.PhyloExplorer.Layout
  ( layout
  ) where

import Gargantext.Prelude

import DOM.Simple (document, querySelector, window)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String (null)
import Data.Tuple.Nested ((/\))
import FFI.Simple ((..), (.=))
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.PhyloExplorer.Resources (PubSubEvent(..))
import Gargantext.Components.PhyloExplorer.Resources as RS
import Gargantext.Components.PhyloExplorer.SideBar (sideBar)
import Gargantext.Components.PhyloExplorer.ToolBar (toolBar)
import Gargantext.Components.PhyloExplorer.TopBar (topBar)
import Gargantext.Components.PhyloExplorer.Types (DisplayView(..), PhyloDataSet(..), ExtractedTerm, ExtractedCount, Source, Term, sortSources)
import Gargantext.Hooks.FirstEffect (useFirstEffect')
import Gargantext.Hooks.UpdateEffect (useUpdateEffect1')
import Gargantext.Types (NodeID)
import Gargantext.Utils (getter, (?))
import Gargantext.Utils.Reactix as R2
import Graphics.D3.Base (d3)
import Reactix as R
import Reactix.DOM.HTML as H
import Simple.JSON as JSON
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.PhyloExplorer"

type Props =
  ( phyloDataSet :: PhyloDataSet
  , nodeId       :: NodeID
  )

layout :: R2.Leaf Props
layout = R2.leaf layoutCpt

layoutCpt :: R.Component Props
layoutCpt = here.component "layout" cpt where
  cpt { phyloDataSet: (PhyloDataSet o)
      , nodeId
      } _ = do
  -- States
  ---------

    let defaultDisplayView = HeadingMode
    let topBarPortalKey = "portal-topbar::" <> show nodeId

    mTopBarHost <- R.unsafeHooksEffect $ R2.getElementById "portal-topbar"

    isDisplayed /\ isReadyBox <-
      R2.useBox' false

    source  /\ sourceBox  <-
      R2.useBox' ""

    sources /\ sourcesBox <-
      R2.useBox' (mempty :: Array Source)

    terms /\ termsBox <-
      R2.useBox' (mempty :: Array Term)

    toolBarDisplayed /\ toolBarDisplayedBox <-
      R2.useBox' false

    search /\ searchBox <-
      R2.useBox' ""

    result /\ resultBox <-
      R2.useBox' (Nothing :: Maybe Term)

    displayView /\ displayViewBox <-
      R2.useBox' defaultDisplayView

    isIsolineDisplayed /\ isIsolineDisplayedBox <-
      R2.useBox' false

    sideBarDisplayed /\ sideBarDisplayedBox <-
      R2.useBox' false

    extractedTerms /\ extractedTermsBox <-
      R2.useBox' (mempty :: Array ExtractedTerm)

    selectedTerm /\ selectedTermBox <-
      R2.useBox' (Nothing :: Maybe String)

    selectedBranch /\ selectedBranchBox <-
      R2.useBox' (Nothing :: Maybe String)

    selectedSource /\ selectedSourceBox <-
      R2.useBox' (Nothing :: Maybe String)

    extractedCount /\ extractedCountBox <-
      R2.useBox' (Nothing :: Maybe ExtractedCount)

  -- Behaviors
  ------------

    -- (!) do not rely on the JavaScript `Resources.js:resetSelection`,
    --     as it will lead to potential circular computations
    resetSelection <- pure $ const do
      T.write_ Nothing selectedBranchBox
      T.write_ Nothing selectedTermBox
      T.write_ ""      sourceBox
      T.write_ Nothing selectedSourceBox
      T.write_ Nothing extractedCountBox
      T.write_ mempty  extractedTermsBox

      void $ pure $ (window .= "branchFocus") []

    changeViewCallback <- pure $
          flip T.write displayViewBox
      >=> RS.changeDisplayView

    sourceCallback <- pure \id -> do
      -- (!) upcoming value from a `B.formSelect`, so simple <String> format
      let mSource = RS.findSourceById sources =<< Int.fromString id
      let mLabel  = pure <<< getter _.label =<< mSource

      resetSelection unit
      T.write_ id sourceBox
      T.write_ mLabel selectedSourceBox
      RS.selectSource window mSource

    searchCallback <- pure $
          flip T.write searchBox
      >=> RS.autocompleteSearch terms
      >=> flip T.write_ resultBox

    resultCallback <- pure $ const $
          resetSelection unit
       *> RS.autocompleteSubmit displayViewBox result

    unselectCallback <- pure $ const $
          resetSelection unit
       *> RS.doubleClick

    selectTermCallback <- pure $ \s -> do
      resetSelection unit
      mTerm <- RS.autocompleteSearch terms s
      RS.autocompleteSubmit displayViewBox mTerm

  -- Effects
  ----------

    -- Drawing the phylo
    useFirstEffect' do
      (sortSources >>> flip T.write_ sourcesBox) o.sources
      RS.setGlobalD3Reference window d3
      RS.setGlobalDependencies window (PhyloDataSet o)
      RS.drawPhylo
        o.branches
        o.periods
        o.groups
        o.links
        o.ancestorLinks
        o.branchLinks
        o.bb
      RS.changeDisplayView displayView
      T.write_ true isReadyBox
      -- @NOTE #219: handling global variables
      --             (see `Resources.js` how they are being used)
      T.write_ (window .. "terms") termsBox

    -- (see `Gargantext.Components.PhyloExplorer.Resources` > JavaScript >
    -- `pubsub` for detailed explanations)
    useFirstEffect' do
      RS.subscribe (show ExtractedTermsEvent) $ flip T.write_ extractedTermsBox

      RS.subscribe (show SelectedTermEvent) $ case _ of
        res
          | true == null res -> T.write_ Nothing selectedTermBox
          | otherwise        -> T.write_ (Just res) selectedTermBox

      RS.subscribe (show SelectedBranchEvent) $ case _ of
        res
          | true == null res -> T.write_ Nothing selectedBranchBox
          | otherwise        -> T.write_ (Just res) selectedBranchBox

      RS.subscribe (show SelectedSourceEvent) $ sourceCallback

      RS.subscribe (show DisplayViewEvent) $ read >>> case _ of
        Nothing  -> pure unit
        Just res -> T.write_ res displayViewBox

      RS.subscribe (show ExtractedCountEvent) $ JSON.readJSON >>> case _ of
        Left _ ->
          T.write_ Nothing extractedCountBox
        Right (res :: ExtractedCount) ->
          T.write_ (Just res) extractedCountBox

    R.useEffect1' isIsolineDisplayed do
      mEl <- querySelector document ".phylo-isoline"
      case mEl of
        Nothing -> pure unit
        Just el -> do
          style <- pure $ (el .. "style")
          pure $ (style .= "display") $
            isIsolineDisplayed ? "flex" $ "none"

    -- @NOTE #219: handling global variables (eg. via `window`)
    --             (see `Resources.js` how they are being used)
    useUpdateEffect1' displayView do
      pure $ (window .= "displayView") (show displayView)

  -- Render
  ---------

    pure $

      H.div
      { className: intercalate " "
          [ "phylo"
          , not isDisplayed ? "phylo--preloading" $ ""
          ]
      }
      [
        -- Preloading spinner
        R2.if' (not isDisplayed) $

          H.div
          { className: "phylo__spinner-wrapper" }
          [
            B.spinner
            { className: "phylo__spinner" }
          ]
      ,
        -- Topbar
        R2.createPortal' mTopBarHost
        [
          R2.fragmentWithKey topBarPortalKey
          [
            R2.if' (isDisplayed) $
              topBar
              { sources
              , source
              , sourceCallback
              , search
              , searchCallback
              , result
              , resultCallback
              , toolBar: toolBarDisplayedBox
              , sideBar: sideBarDisplayedBox
              }
          ]
        ]
      ,
        -- Sidebar
        H.div
        { className: "phylo__sidebar"
        -- @XXX: ReactJS lack of "keep-alive" feature workaround solution
        -- @link https://github.com/facebook/react/issues/12039
        , style: { display: sideBarDisplayed ? "block" $ "none" }
        }
        [
          sideBar
          { nodeId
          , docCount: o.nbDocs
          , foundationCount: o.nbFoundations
          , periodCount: o.nbPeriods
          , termCount: o.nbTerms
          , groupCount: o.nbGroups
          , branchCount: o.nbBranches
          , extractedTerms
          , extractedCount
          , selectedTerm
          , selectedBranch
          , selectedSource
          , selectTermCallback
          }
        ]
      ,
        -- Toolbar
        R2.if' (toolBarDisplayed) $
          toolBar
          { resetViewCallback: const RS.resetView
          , exportCallback: const RS.exportViz
          , unselectCallback: unselectCallback
          , displayView
          , changeViewCallback
          , isolineBox: isIsolineDisplayedBox
          }
      ,
        -- Iso Line
        H.div
        { className: "phylo-isoline"}
        []
      ,
        -- (?) prefer div "margin" instead of CSS margin, it will ease
        --     some computation made on the scape and peak
        H.div
        { className: "phylo-margin" }
        []
      ,
        -- Phylo Grid
        H.div
        { className: "phylo-grid" }
        [
          H.div
          { className: "phylo-grid__blueprint" }
          [
            H.div
            { className: "phylo-grid__blueprint__left" }
            []
          ,
            H.div
            { className: "phylo-grid__blueprint__center" }
            []
          ,
            H.div
            { className: "phylo-grid__blueprint__right"}
            []
          ]
        ,
          H.div
          { className: "phylo-grid__content" }
          [
            H.div
            { className: "phylo-grid__content__scape" }
            []
          ]
        ]
      ,
        -- (?) prefer div "margin" instead of CSS margin, it will ease
        --     some computation made on the scape and peak
        H.div
        { className: "phylo-margin" }
        []
      ]
