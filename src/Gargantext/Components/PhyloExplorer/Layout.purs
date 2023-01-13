module Gargantext.Components.PhyloExplorer.Layout
  ( layout
  ) where

import Gargantext.Prelude

import DOM.Simple (document, querySelector, window)
import Data.Either (Either(..))
import Data.Foldable (for_, intercalate)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String (null)
import Effect (Effect)
import FFI.Simple ((..), (.=))
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.PhyloExplorer.Frame.DocFocus (docFocus)
import Gargantext.Components.PhyloExplorer.Resources (PubSubEvent(..))
import Gargantext.Components.PhyloExplorer.Resources as RS
import Gargantext.Components.PhyloExplorer.SideBar (sideBar)
import Gargantext.Components.PhyloExplorer.Store as PhyloStore
import Gargantext.Components.PhyloExplorer.ToolBar (toolBar)
import Gargantext.Components.PhyloExplorer.TopBar (topBar)
import Gargantext.Components.PhyloExplorer.Types (DisplayView, ExtractedCount, FrameDoc, PhyloData(..), TabView(..), Term, sortSources)
import Gargantext.Hooks.FirstEffect (useFirstEffect')
import Gargantext.Hooks.UpdateEffect (useUpdateEffect1', useUpdateEffect3')
import Gargantext.Types (SidePanelState(..))
import Gargantext.Utils (getter, (?))
import Gargantext.Utils.Reactix as R2
import Graphics.D3.Base (d3)
import Reactix as R
import Reactix.DOM.HTML as H
import Simple.JSON as JSON
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.PhyloExplorer"

layout :: R2.Leaf ()
layout = R2.leaf layoutCpt

layoutCpt :: R.Component ()
layoutCpt = here.component "layout" cpt where
  cpt _ _ = do
    -- | States
    -- |
    { isBuilt
    , source
    , sources
    , terms
    , toolBarDisplayed
    , search
    , result
    , displayView
    , isIsolineDisplayed
    , sideBarDisplayed
    , sideBarTabView
    , extractedTerms
    , selectedTerm
    , selectedBranch
    , selectedSource
    , extractedCount
    , phyloId
    , phyloData
    , frameDoc
    } <- PhyloStore.use

    (PhyloData o)       <- R2.useLive' phyloData
    phyloId'            <- R2.useLive' phyloId
    sources'            <- R2.useLive' sources
    terms'              <- R2.useLive' terms
    displayView'        <- R2.useLive' displayView
    isIsolineDisplayed' <- R2.useLive' isIsolineDisplayed
    sideBarDisplayed'   <- R2.useLive' sideBarDisplayed
    toolBarDisplayed'   <- R2.useLive' toolBarDisplayed
    isBuilt'            <- R2.useLive' isBuilt
    selectedTerm'       <- R2.useLive' selectedTerm
    selectedBranch'     <- R2.useLive' selectedBranch
    selectedSource'     <- R2.useLive' selectedSource
    frameDoc'           <- R2.useLive' frameDoc

    -- | Hooks
    -- |
    let topBarPortalKey = "portal-topbar::" <> show phyloId'

    mTopBarHost <- R.unsafeHooksEffect $ R2.getElementById "portal-topbar"

    -- | Behaviors
    -- |

    -- (!) do not rely on the JavaScript `Resources.js:resetSelection`,
    --     as it will lead to potential circular computations
    --
    -- (!) same idea for Box content mutation: eg. due the reset notion
    --     triggered at each selection (cf. `resetSelection`), a call via
    --     `T.write` + `T.listen` will also create an infinite loop
    let

      resetSelection :: Unit -> Effect Unit
      resetSelection _ = do
        T.write_ Nothing selectedBranch
        T.write_ Nothing selectedTerm
        T.write_ ""      source
        T.write_ Nothing selectedSource
        T.write_ Nothing extractedCount
        T.write_ mempty  extractedTerms

        void $ pure $ (window .= "branchFocus") []

      changeViewCallback :: DisplayView -> Effect Unit
      changeViewCallback =
            flip T.write displayView
        >=> RS.changeDisplayView

      sourceCallback :: String -> Effect Unit
      sourceCallback id = do
        -- (!) upcoming value from a `B.formSelect`, so simple <String> format
        let mSource = RS.findSourceById sources' =<< Int.fromString id
        let mLabel  = pure <<< getter _.label =<< mSource

        resetSelection unit
        T.write_ id source
        T.write_ mLabel selectedSource
        RS.selectSource window mSource

      searchCallback :: String -> Effect Unit
      searchCallback =
            flip T.write search
        >=> RS.autocompleteSearch terms'
        >=> flip T.write_ result

      resultCallback :: Maybe Term -> Effect Unit
      resultCallback mTerm =
            resetSelection unit
        *> RS.autocompleteSubmit displayView mTerm

      unselectCallback :: Unit -> Effect Unit
      unselectCallback _ =
            resetSelection unit
        *> RS.doubleClick

      selectTermCallback :: String -> Effect Unit
      selectTermCallback s = do
        resetSelection unit
        mTerm <- RS.autocompleteSearch terms' s
        RS.autocompleteSubmit displayView mTerm

      closeDocCallback :: Unit -> Effect Unit
      closeDocCallback _ = T.write_ Nothing frameDoc

    -- | Effects
    -- |

    -- Drawing the phylo
    useFirstEffect' do
      (sortSources >>> flip T.write_ sources) o.sources
      RS.setGlobalD3Reference window d3
      RS.setGlobalDependencies window (PhyloData o)
      RS.drawPhylo
        o.branches
        o.periods
        o.groups
        o.links
        o.ancestorLinks
        o.branchLinks
        o.bb
      RS.changeDisplayView displayView'
      T.write_ true isBuilt
      -- @NOTE #219: handling global variables
      --             (see `Resources.js` how they are being used)
      T.write_ (window .. "terms") terms

    -- (see `Gargantext.Components.PhyloExplorer.Resources` > JavaScript >
    -- `pubsub` for detailed explanations)
    useFirstEffect' do
      RS.subscribe (show ExtractedTermsEvent) $ flip T.write_ extractedTerms

      RS.subscribe (show SelectedTermEvent) $ case _ of
        res
          | true == null res -> T.write_ Nothing selectedTerm
          | otherwise        -> T.write_ (Just res) selectedTerm

      RS.subscribe (show SelectedBranchEvent) $ case _ of
        res
          | true == null res -> T.write_ Nothing selectedBranch
          | otherwise        -> T.write_ (Just res) selectedBranch

      RS.subscribe (show SelectedSourceEvent) $ sourceCallback

      RS.subscribe (show DisplayViewEvent) $ read >>> case _ of
        Nothing  -> R.nothing
        Just res -> T.write_ res displayView

      RS.subscribe (show ExtractedCountEvent) $ JSON.readJSON >>> case _ of
        Left _ ->
          T.write_ Nothing extractedCount
        Right (res :: ExtractedCount) ->
          T.write_ (Just res) extractedCount

    R.useEffect1' isIsolineDisplayed' do
      mEl <- querySelector document ".phylo-isoline"
      for_ mEl \el -> do
        let style = el .. "style"
        pure $ (style .= "display") $ isIsolineDisplayed' ?
          "flex" $
          "none"

    -- @NOTE #219: handling global variables (eg. via `window`)
    --             (see `Resources.js` how they are being used)
    useUpdateEffect1' displayView' do
      pure $ (window .= "displayView") (show displayView')

    -- Educational behavior: automatically opening sidebar on first selection
    useUpdateEffect3'
      selectedTerm'
      selectedBranch'
      selectedSource' $
        when (sideBarDisplayed' == InitialClosed) do
          T.write_ Opened sideBarDisplayed
          T.write_ SelectionTab sideBarTabView


    -- | Render
    -- |

    pure $

      H.div
      { className: intercalate " "
          [ "phylo"
          , not isBuilt' ? "phylo--preloading" $ ""
          ]
      }
      [
        -- Preloading spinner
        R2.when (not isBuilt') $

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
            R2.when (isBuilt') $
              topBar
              { sourceCallback
              , searchCallback
              , resultCallback
              }
          ]
        ]
      ,
        -- Sidebar + Focus frame
        H.div
        { className: "phylo__frame" }
        [
          -- Doc focus
          R2.fromMaybe frameDoc' \(frameDoc_ :: FrameDoc) ->

            H.div
            { className: "phylo__focus" }
            [
              H.div
              { className: "phylo__focus__inner" }
              [
                docFocus
                { frameDoc: frameDoc_
                , closeCallback: closeDocCallback
                , key: show $ getter _.docId frameDoc_
                }
              ]
            ]
        ,
          -- Sidebar
          H.div
          { className: "phylo__sidebar"
          -- @XXX: ReactJS lack of "keep-alive" feature workaround solution
          -- @link https://github.com/facebook/react/issues/12039
          , style: { display: sideBarDisplayed' == Opened ? "block" $ "none" }
          }
          [
            H.div
            { className: "phylo__sidebar__inner" }
            [
              sideBar
              { selectTermCallback }
            ]
          ]
        ]
      ,
        -- Toolbar
        R2.when (toolBarDisplayed') $
          toolBar
          { resetViewCallback : const RS.resetView
          , exportCallback    : const RS.exportViz
          , unselectCallback  : unselectCallback
          , changeViewCallback
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
