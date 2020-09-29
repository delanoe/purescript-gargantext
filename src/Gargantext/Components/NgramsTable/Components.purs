module Gargantext.Components.NgramsTable.Components where

import Data.Lens ((^..), (^.), view)
import Data.Lens.At (at)
import Data.Lens.Fold (folded)
import Data.Lens.Index (ix)
import Data.List (null, toUnfoldable) as L
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Nullable (null, toMaybe)
import Data.Set (Set)
import Data.Set as Set
import React.DOM (a, span, text)
import React.DOM.Props as DOM
import Effect (Effect)
import FFI.Simple (delay)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude
import Gargantext.Components.NgramsTable.Core (Action(..), Dispatch, NgramsElement, NgramsPatch(..), NgramsTable, NgramsTablePatch, NgramsTerm, Replace, _NgramsElement, _NgramsRepoElement, _PatchMap, _children, _list, _ngrams, _occurrences, ngramsTermText, replace, singletonNgramsTablePatch)
import Gargantext.Components.Table as Tbl
import Gargantext.Types as T
import Gargantext.Utils.Reactix as R2

thisModule = "Gargantext.Components.NgramsTable.Components"


type SearchInputProps =
  (
    key :: String  -- to prevent refreshing & losing input
  , onSearch :: String -> Effect Unit
  , searchQuery :: String
  )

searchInput :: Record SearchInputProps -> R.Element
searchInput props = R.createElement searchInputCpt props []

searchInputCpt :: R.Component SearchInputProps
searchInputCpt = R2.hooksComponent thisModule "searchInput" cpt
  where
    cpt { onSearch, searchQuery } _ = do
      pure $ H.div { className: "input-group" } [
        H.div { className: "input-group-addon" } [
           H.span { className: "fa fa-search" } []
         ]
      , H.input { className: "form-control"
                , defaultValue: searchQuery
                , name: "search"
                , on: { input: onSearch <<< R2.unsafeEventValue }
                , placeholder: "Search"
                , type: "value" }
        ]


type SelectionCheckboxProps =
  (
    allNgramsSelected :: Boolean
  , dispatch          :: Dispatch
  , ngramsSelection   :: Set NgramsTerm
  )

selectionCheckbox :: Record SelectionCheckboxProps -> R.Element
selectionCheckbox props = R.createElement selectionCheckboxCpt props []

selectionCheckboxCpt :: R.Component SelectionCheckboxProps
selectionCheckboxCpt = R2.hooksComponent thisModule "selectionCheckbox" cpt
  where
    cpt { allNgramsSelected, dispatch, ngramsSelection } _ = do
      ref <- R.useRef null

      R.useEffect' $ delay unit $ \_ -> do
        let mCb = toMaybe $ R.readRef ref
        case mCb of
          Nothing -> pure unit
          Just cb -> do
            _ <- if allNgramsSelected || (Set.isEmpty ngramsSelection) then
              R2.setIndeterminateCheckbox cb false
            else
              R2.setIndeterminateCheckbox cb true
            pure unit

      pure $ H.input { checked: allNgramsSelected
                     , className: "checkbox"
                     , on: { change: const $ dispatch $ ToggleSelectAll }
                     , ref
                     , type: "checkbox" }


type RenderNgramsTree =
  ( ngrams      :: NgramsTerm
  , ngramsClick :: NgramsClick
  , ngramsEdit  :: NgramsClick
  , ngramsStyle :: Array DOM.Props
  , ngramsTable :: NgramsTable
  )

renderNgramsTree :: Record RenderNgramsTree -> R.Element
renderNgramsTree p = R.createElement renderNgramsTreeCpt p []

renderNgramsTreeCpt :: R.Component RenderNgramsTree
renderNgramsTreeCpt = R2.hooksComponent thisModule "renderNgramsTree" cpt
  where
    cpt { ngramsTable, ngrams, ngramsStyle, ngramsClick, ngramsEdit } _ =
      pure $ H.ul {} [
        H.span { className: "tree" } [
          H.span { className: "righthanded" } [
            tree { ngramsClick
                 , ngramsDepth: {ngrams, depth: 0}
                 , ngramsEdit
                 , ngramsStyle
                 , ngramsTable
                 }
          ]
        ]
      ]


type NgramsDepth = {ngrams :: NgramsTerm, depth :: Int}
type NgramsClick = NgramsDepth -> Maybe (Effect Unit)

type TreeProps =
  (
    ngramsClick :: NgramsClick
  , ngramsDepth :: NgramsDepth
  , ngramsEdit  :: NgramsClick
  , ngramsStyle :: Array DOM.Props
  , ngramsTable :: NgramsTable
  )

tree :: Record TreeProps -> R.Element
tree p = R.createElement treeCpt p []

treeCpt :: R.Component TreeProps
treeCpt = R2.hooksComponent thisModule "tree" cpt
  where
    cpt params@{ ngramsClick, ngramsDepth, ngramsEdit, ngramsStyle, ngramsTable } _ =
      pure $
        H.li { style: {width : "100%"} }
          ([ H.i { className, style } [] ]
           <> [ R2.buff $ tag [ text $ " " <> ngramsTermText ngramsDepth.ngrams ] ]
           <> maybe [] edit (ngramsEdit ngramsDepth)
           <> [ forest cs ])
      where
        tag =
          case ngramsClick ngramsDepth of
            Just effect ->
              a (ngramsStyle <> [DOM.onClick $ const effect])
            Nothing ->
              span ngramsStyle
        edit effect = [ H.text " "
                      , H.i { className: "glyphicon glyphicon-pencil"
                            , on: { click: const effect } } []
                      ]
        leaf = L.null cs
        className = "glyphicon glyphicon-chevron-" <> if open then "down" else "right"
        style = if leaf then {color: "#adb5bd"} else {color: ""}
        open = not leaf || false {- TODO -}
        cs   = ngramsTable ^.. ix ngramsDepth.ngrams <<< _NgramsRepoElement <<< _children <<< folded

        forest =
          let depth = ngramsDepth.depth + 1 in
          if depth > 10 then
            const $ H.text "ERROR DEPTH > 10"
          else
            H.ul {} <<< map (\ngrams -> tree (params { ngramsDepth = {depth, ngrams} })) <<< L.toUnfoldable


type RenderNgramsItem =
  ( dispatch :: Action -> Effect Unit
  , ngrams :: NgramsTerm
  , ngramsElement :: NgramsElement
  , ngramsLocalPatch :: NgramsTablePatch
  , ngramsParent :: Maybe NgramsTerm
  , ngramsSelection :: Set NgramsTerm
  , ngramsTable :: NgramsTable
  )

renderNgramsItem :: Record RenderNgramsItem -> R.Element
renderNgramsItem p = R.createElement renderNgramsItemCpt p []

renderNgramsItemCpt :: R.Component RenderNgramsItem
renderNgramsItemCpt = R2.hooksComponent thisModule "renderNgramsItem" cpt
  where
    cpt { dispatch
        , ngrams
        , ngramsElement
        , ngramsLocalPatch
        , ngramsParent
        , ngramsSelection
        , ngramsTable } _ =
      pure $ Tbl.makeRow [
          selected
        , checkbox T.MapTerm
        , checkbox T.StopTerm
        , if ngramsParent == Nothing
          then renderNgramsTree { ngramsTable, ngrams, ngramsStyle, ngramsClick, ngramsEdit }
          else
            H.a { on: { click: const $ dispatch $ ToggleChild true ngrams } } [
                H.i { className: "glyphicon glyphicon-plus" } []
              , (R2.buff $ span ngramsStyle [text $ " " <> ngramsTermText ngrams])
            ]
        , H.text $ show (ngramsElement ^. _NgramsElement <<< _occurrences)
      ]
      where
        termList    = ngramsElement ^. _NgramsElement <<< _list
        ngramsStyle = [termStyle termList ngramsOpacity]
        ngramsEdit  = Just <<< dispatch <<< SetParentResetChildren <<< Just <<< view _ngrams
        ngramsClick
          = Just <<< dispatch <<< cycleTermListItem <<< view _ngrams
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
                  , type: "checkbox" }
        checkbox termList' =
          let chkd = termList == termList'
              termList'' = if chkd then T.CandidateTerm else termList'
          in
          H.input { checked: chkd
                  , className: "checkbox"
                  , on: { change: const $ dispatch $
                          setTermListA ngrams (replace termList termList'') }
                  , readOnly: ngramsTransient
                  , type: "checkbox" }
        ngramsTransient = tablePatchHasNgrams ngramsLocalPatch ngrams
          -- ^ TODO here we do not look at ngramsNewElems, shall we?
        ngramsOpacity
          | ngramsTransient = 0.5
          | otherwise       = 1.0

        cycleTermListItem n = setTermListA n (replace termList (nextTermList termList))


termStyle :: T.TermList -> Number -> DOM.Props
termStyle T.MapTerm     opacity = DOM.style { color: "green", opacity }
termStyle T.StopTerm      opacity = DOM.style { color: "red",   opacity
                                              , textDecoration: "line-through" }
termStyle T.CandidateTerm opacity = DOM.style { color: "black", opacity }

setTermListA :: NgramsTerm -> Replace T.TermList -> Action
setTermListA n patch_list =
  CommitPatch $
    singletonNgramsTablePatch n $
    NgramsPatch { patch_list, patch_children: mempty }


tablePatchHasNgrams :: NgramsTablePatch -> NgramsTerm -> Boolean
tablePatchHasNgrams ngramsTablePatch ngrams =
  isJust $ ngramsTablePatch.ngramsPatches ^. _PatchMap <<< at ngrams


nextTermList :: T.TermList -> T.TermList
nextTermList T.MapTerm     = T.StopTerm
nextTermList T.StopTerm      = T.CandidateTerm
nextTermList T.CandidateTerm = T.MapTerm
