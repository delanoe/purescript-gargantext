module Gargantext.Components.NgramsTable.Components where

import Data.Lens ((^..), (^.), view)
import Data.Lens.At (at)
import Data.Lens.Fold (folded)
import Data.Lens.Index (ix)
import Data.List (List)
import Data.List (null, toUnfoldable) as L
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Nullable (null, toMaybe)
import Data.Set (Set)
import Data.Set as Set
import Effect (Effect)
import FFI.Simple (delay)
import Gargantext.Components.NgramsTable.Core (Action(..), Dispatch, NgramsElement, NgramsTable, NgramsTablePatch, NgramsTerm, _NgramsElement, _NgramsRepoElement, _PatchMap, _children, _list, _ngrams, _occurrences, ngramsTermText, replace, setTermListA)
import Gargantext.Components.Table as Tbl
import Gargantext.Prelude (Unit, bind, const, discard, map, not, otherwise, pure, show, unit, ($), (+), (/=), (<<<), (<>), (==), (>), (||))
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2
import React.DOM (a, span, text)
import React.DOM.Props as DOM
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.NgramsTable.Components"

type SearchInputProps =
  ( searchQuery :: T.Box String
  )

-- "key": to prevent refreshing & losing input
searchInput :: R2.Leaf ( key :: String | SearchInputProps )
searchInput = R2.leafComponent searchInputCpt
searchInputCpt :: R.Component ( key :: String | SearchInputProps )
searchInputCpt = here.component "searchInput" cpt
  where
    cpt { searchQuery } _ = do
      pure $ R2.row
        [ H.div { className: "col-12" }
          [ H.div { className: "input-group" }
            [ searchButton { searchQuery } []
            , searchFieldInput { searchQuery } []
            ]
          ]
        ]

type SearchButtonProps =
  ( searchQuery :: T.Box String
  )

searchButton :: R2.Component SearchButtonProps
searchButton = R.createElement searchButtonCpt
searchButtonCpt :: R.Component SearchButtonProps
searchButtonCpt = here.component "searchButton" cpt where
  cpt { searchQuery } _ = do
    searchQuery' <- T.useLive T.unequal searchQuery

    pure $ H.div { className: "input-group-prepend" }
      [ if searchQuery' /= ""
        then
          H.button { className: "btn btn-danger"
                   , on: {click: \_ -> T.write "" searchQuery}}
          [ H.span {className: "fa fa-times"} []]
        else H.span { className: "fa fa-search input-group-text" } []
      ]

type SearchFieldInputProps =
  ( searchQuery :: T.Box String
  )

searchFieldInput :: R2.Component SearchFieldInputProps
searchFieldInput = R.createElement searchFieldInputCpt
searchFieldInputCpt :: R.Component SearchFieldInputProps
searchFieldInputCpt = here.component "searchFieldInput" cpt where
  cpt { searchQuery } _ = do
    -- searchQuery' <- T.useLive T.unequal searchQuery

    pure $ H.input { className: "form-control"
                   -- , defaultValue: searchQuery'
                   , name: "search"
                   , on: { input: \e -> T.write (R.unsafeEventValue e) searchQuery }
                   , placeholder: "Search"
                   , type: "value"
                   }

type SelectionCheckboxProps =
  ( allNgramsSelected :: Boolean
  , dispatch          :: Dispatch
  , ngramsSelection   :: Set NgramsTerm
  )

selectionCheckbox :: Record SelectionCheckboxProps -> R.Element
selectionCheckbox props = R.createElement selectionCheckboxCpt props []
selectionCheckboxCpt :: R.Component SelectionCheckboxProps
selectionCheckboxCpt = here.component "selectionCheckbox" cpt
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
  ( ngramsChildren :: List NgramsTerm
  , ngramsClick    :: NgramsClick
  , ngramsDepth    :: NgramsDepth
  , ngramsEdit     :: NgramsClick
  , ngramsStyle    :: Array DOM.Props
  --, ngramsTable    :: NgramsTable
  )

renderNgramsTree :: Record RenderNgramsTree -> R.Element
renderNgramsTree p = R.createElement renderNgramsTreeCpt p []
renderNgramsTreeCpt :: R.Component RenderNgramsTree
renderNgramsTreeCpt = here.component "renderNgramsTree" cpt
  where
    cpt { ngramsChildren, ngramsClick, ngramsDepth, ngramsEdit, ngramsStyle } _ =
      pure $ H.ul {}
      [ H.span { className: "tree" }
        [ H.span { className: "righthanded" }
          [ tree { ngramsChildren
                 , ngramsClick
                 , ngramsDepth
                 , ngramsEdit
                 , ngramsStyle
                 }
          ]
        ]
      ]


type NgramsDepth = { ngrams :: NgramsTerm, depth :: Int }
type NgramsClick = NgramsDepth -> Maybe (Effect Unit)

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
  ( ngramsChildren :: List NgramsTerm
  , ngramsEdit     :: NgramsClick
  --, ngramsTable :: NgramsTable
  | TagProps
  )

tree :: Record TreeProps -> R.Element
tree p = R.createElement treeCpt p []
treeCpt :: R.Component TreeProps
treeCpt = here.component "tree" cpt
  where
    cpt params@{ ngramsChildren, ngramsClick, ngramsDepth, ngramsEdit, ngramsStyle } _ = do

      R.useEffect' $ do
        here.log2 "[tree] ngramsChildren" ngramsChildren
      
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
            H.ul {} <<< map (\ngrams -> tree (params { ngramsDepth = {depth, ngrams} })) <<< L.toUnfoldable


type RenderNgramsItem = (
    dispatch          :: Action -> Effect Unit
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
        , ngrams
        , ngramsElement
        , ngramsLocalPatch
        , ngramsParent
        , ngramsSelection
        , ngramsTable
        } _ = do
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
            then [renderNgramsTree { ngramsChildren
                                   , ngramsClick
                                   , ngramsDepth
                                   , ngramsEdit
                                   , ngramsStyle }]
            else [H.a { on: { click: const $ dispatch $ ToggleChild true ngrams } }
                  [ H.i { className: "fa fa-plus" } []]
                 , R2.buff $ tag [ text $ " " <> ngramsTermText ngramsDepth.ngrams ]
                 ]
          )
        , H.text $ show (ngramsElement ^. _NgramsElement <<< _occurrences)
      ]
      where
        ngramsDepth= { ngrams, depth: 0 }
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
        ngramsChildren = ngramsTable ^.. ix ngrams <<< _NgramsRepoElement <<< _children <<< folded
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

tablePatchHasNgrams :: NgramsTablePatch -> NgramsTerm -> Boolean
tablePatchHasNgrams ngramsTablePatch ngrams =
  isJust $ ngramsTablePatch.ngramsPatches ^. _PatchMap <<< at ngrams


nextTermList :: GT.TermList -> GT.TermList
nextTermList GT.MapTerm       = GT.StopTerm
nextTermList GT.StopTerm      = GT.CandidateTerm
nextTermList GT.CandidateTerm = GT.MapTerm
