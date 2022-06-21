module Gargantext.Components.NgramsTable.Tree where

import Data.Array as A
import Data.Either (Either(..))
import Data.Lens ((^..), (^.), view)
import Data.Lens.At (at)
import Data.Lens.Fold (folded)
import Data.Lens.Index (ix)
import Data.List (List)
import Data.List as L
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Nullable (Nullable, null, toMaybe)
import Data.Set (Set)
import Data.Set as Set
import DOM.Simple as DOM
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Gargantext.Core.NgramsTable.Functions (applyNgramsPatches, setTermListA, tablePatchHasNgrams)
import Gargantext.Core.NgramsTable.Types (Action(..), NgramsClick, NgramsDepth, NgramsElement, NgramsTable, NgramsTablePatch(..), NgramsTerm, _NgramsElement, _NgramsRepoElement, _PatchMap, _children, _list, _ngrams, _occurrences, ngramsTermText, replace)
import Gargantext.Components.Table as Tbl
import Gargantext.Config.REST (logRESTError)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Prelude (Unit, bind, const, discard, map, mempty, not, otherwise, pure, show, unit, ($), (+), (/=), (<<<), (<>), (==), (>), (||))
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import React.DOM (a, span, text)
import React.DOM.Props as DOM
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Toestand as T
import Type.Proxy (Proxy(..))

here :: R2.Here
here = R2.here "Gargantext.Components.NgramsTable.Tree"


type RenderNgramsTree =
  ( getNgramsChildren :: NgramsTerm -> Aff (Array NgramsTerm)
  --, ngramsChildren    :: List NgramsTerm
  , ngramsClick       :: NgramsClick
  , ngramsDepth       :: NgramsDepth
  , ngramsEdit        :: NgramsClick
  , ngramsStyle       :: Array DOM.Props
  --, ngramsTable    :: NgramsTable
  , key               :: String -- used to refresh the tree on diff change
  )

renderNgramsTree :: Record RenderNgramsTree -> R.Element
renderNgramsTree p = R.createElement renderNgramsTreeCpt p []
renderNgramsTreeCpt :: R.Component RenderNgramsTree
renderNgramsTreeCpt = here.component "renderNgramsTree" cpt
  where
    cpt { getNgramsChildren, ngramsClick, ngramsDepth, ngramsEdit, ngramsStyle } _ = do
      pure $ H.ul {}
        [ H.span { className: "tree" }
          [ H.span { className: "righthanded" }
            [ tree { getNgramsChildren
                     --, ngramsChildren
                   , ngramsClick
                   , ngramsDepth
                   , ngramsEdit
                   , ngramsStyle
                   }
            ]
          ]
        ]


type TagProps =
  ( ngramsClick :: NgramsClick
  , ngramsDepth :: NgramsDepth
  , ngramsStyle :: Array DOM.Props
  )

{- TODO refactor here
-- tag :: TagProps -> Array R.Element -> R.Element
tag tagProps =
  case tagProps.ngramsClick tagProps.ngramsDepth of
    Just effect ->
      a (tagProps.ngramsStyle <> [DOM.onClick $ const effect])
    Nothing ->
      span tagProps.ngramsStyle
-}

type TreeProps =
  ( getNgramsChildren :: NgramsTerm -> Aff (Array NgramsTerm)
  , ngramsEdit        :: NgramsClick
  --, ngramsTable :: NgramsTable
  | TagProps
  )

tree :: Record TreeProps -> R.Element
tree p = R.createElement treeCpt p []
treeCpt :: R.Component TreeProps
treeCpt = here.component "tree" cpt where
  cpt props@{ getNgramsChildren, ngramsDepth } _ = do
    let loader p = do
          res <- getNgramsChildren p
          pure $ Right res
    let render nc = treeLoaded (Record.merge props { ngramsChildren: L.fromFoldable nc })

    useLoader { errorHandler
              , loader
              , path: ngramsDepth.ngrams
              , render }
    where
      errorHandler = logRESTError here "[tree]"

type TreeLoaded =
  ( ngramsChildren    :: List NgramsTerm
  | TreeProps )

treeLoaded :: Record TreeLoaded -> R.Element
treeLoaded p = R.createElement treeLoadedCpt p []
treeLoadedCpt :: R.Component TreeLoaded
treeLoadedCpt = here.component "treeLoaded" cpt where
  cpt params@{ ngramsChildren, ngramsClick, ngramsDepth, ngramsEdit, ngramsStyle } _ = do
    pure $
      H.li { style: { width : "100%" } }
      ([ H.i { className, style } [] ]
       <> [ R2.buff $ tag [ text $ " " <> ngramsTermText ngramsDepth.ngrams ] ]
       <> maybe [] edit (ngramsEdit ngramsDepth)
       <> [ forest ngramsChildren ]
      )
    where
      tag =
        case ngramsClick ngramsDepth of
          Just effect ->
            a (ngramsStyle <> [DOM.onClick $ const effect])
          Nothing ->
            span ngramsStyle
      edit effect = [ H.text " "
                    , H.i { className: "fa fa-pencil"
                          , on: { click: const effect } } []
                    ]
      leaf = L.null ngramsChildren
      className = "fa fa-chevron-" <> if open then "down" else "right"
      style = if leaf then {color: "#adb5bd"} else {color: ""}
      open = not leaf || false {- TODO -}
      --cs   = ngramsTable ^.. ix ngramsDepth.ngrams <<< _NgramsRepoElement <<< _children <<< folded
      -- cs has a list is ok, the length is the number of direct children of an ngram which is generally < 10.

      forest =
        let depth = ngramsDepth.depth + 1 in
        if depth > 10 then
          const $ H.text "ERROR DEPTH > 10"
        else
          H.ul {} <<< map (\ngrams -> tree ((Record.delete (Proxy :: Proxy "ngramsChildren") params) { ngramsDepth = {depth, ngrams} })) <<< L.toUnfoldable

type RenderNgramsItem =
  ( dispatch          :: Action -> Effect Unit
  , getNgramsChildren :: NgramsTerm -> Aff (Array NgramsTerm)
  , ngrams            :: NgramsTerm
  , ngramsElement     :: NgramsElement
  , ngramsLocalPatch  :: NgramsTablePatch
  , ngramsParent      :: Maybe NgramsTerm
  , ngramsSelection   :: Set NgramsTerm
  , ngramsTable       :: NgramsTable
  )

renderNgramsItem :: R2.Component RenderNgramsItem
renderNgramsItem = R.createElement renderNgramsItemCpt
renderNgramsItemCpt :: R.Component RenderNgramsItem
renderNgramsItemCpt = here.component "renderNgramsItem" cpt
  where
    cpt { dispatch
        --, getNgramsChildren
        , ngrams
        , ngramsElement
        , ngramsLocalPatch
        , ngramsParent
        , ngramsSelection
        , ngramsTable
        } _ = do
      R.useEffect' $ do
        here.log2 "[renderNgramsItem] tbl" tbl
      
      pure $ Tbl.makeRow
        [ H.div { className: "ngrams-selector" }
          [ H.span { className: "ngrams-chooser fa fa-eye-slash"
                   , on: { click: onClick } } []
          ]
        , selected
        , checkbox GT.MapTerm
        , checkbox GT.StopTerm
        , H.div {}
          ( if ngramsParent == Nothing
            then [ renderNgramsTree { getNgramsChildren: getNgramsChildren'
                                    , ngramsClick
                                    , ngramsDepth
                                    , ngramsEdit
                                    , ngramsStyle
                                    , key: "" } ]
            else [ H.a { on: { click: const $ dispatch $ ToggleChild true ngrams } }
                   [ H.i { className: "fa fa-plus" } [] ]
                 , R2.buff $ tag [ text $ " " <> ngramsTermText ngramsDepth.ngrams ]
                 ]
          )
        , H.text $ show (ngramsElement ^. _NgramsElement <<< _occurrences)
      ]
      where
        ngramsDepth = { ngrams, depth: 0 }
        tag =
          case ngramsClick ngramsDepth of
            Just effect ->
              a (ngramsStyle <> [DOM.onClick $ const effect])
            Nothing ->
              span ngramsStyle
        onClick _ = pure unit :: Effect Unit
        -- onClick _ = do
        --   R2.callTrigger toggleSidePanel unit
        termList    = ngramsElement ^. _NgramsElement <<< _list
        ngramsStyle = [termStyle termList ngramsOpacity]
        ngramsEdit { ngrams } = Just $ dispatch $ SetParentResetChildren (Just ngrams) ngramsChildren
        tbl = applyNgramsPatches { ngramsLocalPatch
                                 , ngramsStagePatch: mempty
                                 , ngramsValidPatch: mempty
                                 , ngramsVersion: 0 } ngramsTable
        getNgramsChildren' :: NgramsTerm -> Aff (Array NgramsTerm)
        getNgramsChildren' n = if n == ngrams then (pure $ A.fromFoldable ngramsChildren) else pure []
        ngramsChildren = tbl ^.. ix ngrams <<< _NgramsRepoElement <<< _children <<< folded
        ngramsClick =
          Just <<< dispatch <<< CoreAction <<< cycleTermListItem <<< view _ngrams
          -- ^ This is the old behavior it is nicer to use since one can
          --   rapidly change the ngram list without waiting for confirmation.
          --   However this might expose bugs. One of them can be reproduced
          --   by clicking a multiple times on the same ngram, sometimes it stays
          --   transient.
          -- | ngramsTransient = const Nothing
          -- | otherwise       = Just <<< dispatch <<< cycleTermListItem <<< view _ngrams
        selected    =
          H.input { checked: Set.member ngrams ngramsSelection
                  , className: "checkbox"
                  , on: { change: const $ dispatch $ ToggleSelect ngrams }
                  , type: "checkbox"
                  }
        checkbox termList' =
          let chkd = termList == termList'
              termList'' = if chkd then GT.CandidateTerm else termList'
          in
          H.input { checked: chkd
                  , className: "checkbox"
                  , on: { change: const $ dispatch $ CoreAction $
                          setTermListA ngrams (replace termList termList'') }
                  , readOnly: ngramsTransient
                  , type: "checkbox" }
        ngramsTransient = tablePatchHasNgrams ngramsLocalPatch ngrams
          -- ^ TODO here we do not look at ngramsNewElems, shall we?
        ngramsOpacity
          | ngramsTransient = 0.5
          | otherwise       = 1.0

        cycleTermListItem n = setTermListA n (replace termList (nextTermList termList))


termStyle :: GT.TermList -> Number -> DOM.Props
termStyle GT.MapTerm       opacity = DOM.style
  { color: "#11AA11"
  , opacity
  }
termStyle GT.StopTerm      opacity = DOM.style
  { color: "#EE3311"
  , opacity
  , textDecoration: "line-through"
  }
termStyle GT.CandidateTerm opacity = DOM.style
  { color: "#5A90B6"
  , fontStyle: "italic"
  , opacity
  }


nextTermList :: GT.TermList -> GT.TermList
nextTermList GT.MapTerm       = GT.StopTerm
nextTermList GT.StopTerm      = GT.CandidateTerm
nextTermList GT.CandidateTerm = GT.MapTerm
