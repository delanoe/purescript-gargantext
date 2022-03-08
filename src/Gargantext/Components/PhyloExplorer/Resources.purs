module Gargantext.Components.PhyloExplorer.Resources
  ( PubSubEvent(..)
  , drawPhylo
  , selectSource
  , findSourceById
  , autocompleteSearch, autocompleteSubmit
  , setGlobalDependencies, setGlobalD3Reference
  , resetView
  , changeDisplayView
  , exportViz
  , doubleClick
  , subscribe, unsubscribe, publish
  ) where

import Gargantext.Prelude

import DOM.Simple (Element, Window)
import Data.Array as Array
import Data.Foldable (for_)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.String as String
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn4, EffectFn7, runEffectFn1, runEffectFn2, runEffectFn4, runEffectFn7)
import FFI.Simple ((..), (.=), (.?))
import Gargantext.Components.PhyloExplorer.Types (AncestorLink, Branch, BranchLink, DisplayView(..), Group(..), Link, Period, PhyloDataSet(..), Source(..), Term(..))
import Gargantext.Utils (getter)
import Gargantext.Utils.Reactix ((~~))
import Gargantext.Utils.Reactix as R2
import Graphics.D3.Base (D3, D3Eff)
import Graphics.D3.Selection as D3S
import Graphics.D3.Util (ffi)
import Toestand as T


foreign import _drawPhylo :: EffectFn7
  (Array Branch)
  (Array Period)
  (Array Group)
  (Array Link)
  (Array AncestorLink)
  (Array BranchLink)
  (Array Number)
  (Unit)

drawPhylo ::
     Array Branch
  -> Array Period
  -> Array Group
  -> Array Link
  -> Array AncestorLink
  -> Array BranchLink
  -> Array Number
  -> Effect Unit
drawPhylo = runEffectFn7 _drawPhylo


foreign import _drawWordCloud :: forall a. EffectFn1 (Array a) Unit

drawWordCloud :: forall a. Array a -> Effect Unit
drawWordCloud = runEffectFn1 _drawWordCloud


foreign import _termClick ::
  EffectFn4
  String
  String
  Int
  String
  Unit

termClick :: String -> String -> Int -> String -> Effect Unit
termClick = runEffectFn4 _termClick


foreign import _resetView :: Effect Unit

resetView :: Effect Unit
resetView = _resetView


foreign import _showLabel :: Effect Unit

showLabel :: Effect Unit
showLabel = _showLabel


foreign import _showHeading :: Effect Unit

showHeading :: Effect Unit
showHeading = _showHeading


foreign import _showLanding :: Effect Unit

showLanding :: Effect Unit
showLanding = _showLanding


foreign import _exportViz :: Effect Unit

exportViz :: Effect Unit
exportViz = _exportViz


foreign import _doubleClick :: Effect Unit

doubleClick :: Effect Unit
doubleClick = _doubleClick


foreign import _highlightGroups ::
  EffectFn1
  (Array Element)
  Unit

highlightGroups :: Array Element -> Effect Unit
highlightGroups = runEffectFn1 _highlightGroups


foreign import _initPath :: Effect Unit

initPath :: Effect Unit
initPath = _initPath

-----------------------------------------------------------

foreign import _subscribe :: forall opt.
  EffectFn2
  String
  (opt -> Effect Unit)
  Unit

subscribe :: forall opt. String -> (opt -> Effect Unit) -> Effect Unit
subscribe = runEffectFn2 _subscribe


foreign import _unsubscribe ::
  EffectFn2
  String
  String
  Unit

unsubscribe :: String -> String -> Effect Unit
unsubscribe = runEffectFn2 _unsubscribe


foreign import _publish :: forall opt.
  EffectFn2
  String
  opt
  Unit

publish :: forall opt. String -> opt -> Effect Unit
publish = runEffectFn2 _publish

-----------------------------------------------------------

foreign import _extractedTermsEvent     :: String
foreign import _extractedCountEvent     :: String
foreign import _selectedTermEvent       :: String
foreign import _selectedBranchEvent     :: String
foreign import _selectedSourceEvent     :: String
foreign import _displayViewEvent        :: String

data PubSubEvent
  = ExtractedTermsEvent
  | ExtractedCountEvent
  | SelectedTermEvent
  | SelectedBranchEvent
  | SelectedSourceEvent
  | DisplayViewEvent

derive instance Generic PubSubEvent _
derive instance Eq PubSubEvent
instance Show PubSubEvent where
  show = case _ of
    ExtractedTermsEvent     -> _extractedTermsEvent
    ExtractedCountEvent     -> _extractedCountEvent
    SelectedTermEvent       -> _selectedTermEvent
    SelectedBranchEvent     -> _selectedBranchEvent
    SelectedSourceEvent     -> _selectedSourceEvent
    DisplayViewEvent        -> _displayViewEvent

-----------------------------------------------------------

-- @XXX: "Graphics.D3.Selection" lack of "filter" function
-- https://github.com/d3/d3-selection#selection_filter
selectionFilter :: forall d. String -> D3S.Selection d -> D3Eff (D3S.Selection D3S.Void)
selectionFilter = ffi ["query", "selection", ""] "selection.filter(query)"

-- @XXX: "Graphics.D3.Selection" lack of "nodes" function
-- https://github.com/d3/d3-selection#selection_nodes
selectionNodes :: forall d. D3S.Selection d -> D3Eff (Array Element)
selectionNodes = ffi ["selection", ""] "selection.nodes()"

-- @XXX: "Graphics.D3.Selection" lack of "node" function
-- https://github.com/d3/d3-selection#selection_node
-- selectionNode :: forall d. D3S.Selection d -> D3Eff (Nullable Element)
-- selectionNode = ffi ["selection", ""] "selection.node()"

-----------------------------------------------------------

setGlobalDependencies :: Window -> PhyloDataSet -> Effect Unit
setGlobalDependencies w (PhyloDataSet o)
  = do
    _ <- pure $ (w .= "freq") {}
    _ <- pure $ (w .= "nbBranches") o.nbBranches
    _ <- pure $ (w .= "nbDocs") o.nbDocs
    _ <- pure $ (w .= "nbFoundations") o.nbFoundations
    _ <- pure $ (w .= "nbGroups") o.nbGroups
    _ <- pure $ (w .= "nbPeriods") o.nbPeriods
    _ <- pure $ (w .= "nbTerms") o.nbTerms
    _ <- pure $ (w .= "sources") o.sources
    _ <- pure $ (w .= "terms") []
    _ <- pure $ (w .= "timeScale") o.timeScale
    _ <- pure $ (w .= "weighted") o.weighted
    _ <- pure $ (w .= "branchFocus") []

    (freq :: Array Int)   <- pure $ w .. "freq"
    (terms :: Array Term) <- pure $ w .. "terms"

    for_ o.groups \(Group g) -> do

      let
        f = g.foundation
        l = g.label

      forWithIndex_ f \idx val ->
        let
          idx' = show idx
          val' = show val
        in
          -- For each entries in group.foundation array,
          -- increment consequently the global window.keys array
          case (freq .? val') of
            Nothing -> pure $ (freq .= val') 0
            Just v  -> pure $ (freq .= val') (v +1)
        *>
          -- For each entries in group.foundation array,
          -- if the global window.terms does not have it in property,
          -- append an item to the global window.terms
          case (terms .? val') of
            Just _  -> pure unit
            Nothing -> void <<< pure $ (terms .= val') $ Term
              { label: l .. idx'
              , fdt  : val'
              }

    -- Use FFI native `Array.flat` method (not mutating its caller in this
    -- context)
    void do
      new <- pure $ (terms ~~ "flat") []
      pure $ (w .= "terms") new

-- @XXX: prevent PureScript from not injecting D3
setGlobalD3Reference :: Window -> D3 -> Effect Unit
setGlobalD3Reference window d3 = void $ pure $ (window .= "d3") d3

-----------------------------------------------------------

selectSource :: Window -> Maybe Source -> Effect Unit
selectSource window source =
  let
    hasHighlight = maybe false identity (window .? "highlighted")
    hasLdView    = maybe false identity (window .? "ldView")

  in do
    groups <- D3S.rootSelectAll ".group-inner"

    if hasHighlight
    then
          selectionFilter ".source-focus" groups
      >>= selectionNodes
      >>= flip for_ (flip R2.addClass [ "group-unfocus" ])
    else
      pure unit


    -- unselected all the groups
    _ <-  selectionNodes groups
      >>= flip for_ (flip R2.removeClass [ "source-focus" ])

    if hasLdView
    then
          selectionNodes groups
      >>= flip for_ (fill "#f5eee6")
    else
          selectionNodes groups
      >>= flip for_ (fill "#fff")

    _ <-  D3S.rootSelectAll ".peak"
      >>= D3S.classed "peak-focus-source" false


    -- select the relevant ones
    case source of
      Nothing     -> do
        drawWordCloud []

      Just (Source { id }) -> do
        arr <- selectionFilter (".source-" <> show id) groups
          >>= selectionNodes

        drawWordCloud arr
        initPath
        for_ arr $ focusBranch window
        highlightGroups arr
        for_ arr markSourceFocus

  where

    fill :: String -> Element -> Effect Unit
    fill hex el = do
      style <- pure $ (el .. "style")
      pure $ (style .= "fill") hex


    markSourceFocus :: Element -> Effect Unit
    markSourceFocus el = do
      R2.removeClass el [ "group-unfocus" ]
      R2.addClass el [ "source-focus" ]

      bid <- pure $ (el ~~ "getAttribute") [ "bId" ]

      void $
            D3S.rootSelect ("#peak-" <> bid)
        >>= D3S.classed "peak-focus-source" true

    focusBranch :: Window -> Element -> Effect Unit
    focusBranch w el = do
      (branchFocus :: Array Int) <- pure $ w .. "branchFocus"

      bid <- pure $ (el ~~ "getAttribute") [ "bId" ]

      case fromString bid of
        Nothing -> pure unit
        Just i  -> pure $ (branchFocus ~~ "push") [ i ]


findSourceById :: Array Source -> Int -> Maybe Source
findSourceById list value = Array.find (fn) list

  where

    fn (Source { id }) = eq value id


autocompleteSearch ::
     Array Term
  -> String
  -> Effect (Maybe Term)
autocompleteSearch terms query =
  let
    hasMinLen = String.length >>> (_ > 0)

  in pure

    if hasMinLen query
    then findTermByPrefix terms query
    else Nothing


autocompleteSubmit :: T.Box DisplayView -> Maybe Term -> Effect Unit
autocompleteSubmit displayView = case _ of
  Nothing                    -> pure unit
  Just (Term { label, fdt }) -> do
    showLabel
    T.write_ LabelMode displayView
    termClick label fdt 0 "search"


findTermByPrefix :: Array Term -> String -> Maybe Term
findTermByPrefix terms prefix =
  let
    needle = String.toLower prefix
    fn s
        = getter _.label
      >>> String.toLower
      >>> String.stripPrefix (String.Pattern s)
      >>> isJust

  in
    Array.find (fn needle) terms


changeDisplayView :: DisplayView -> Effect Unit
changeDisplayView = case _ of
  LabelMode   -> showLabel
  HeadingMode -> showHeading
  LandingMode -> showLanding
