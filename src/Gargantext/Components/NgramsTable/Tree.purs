module Gargantext.Components.NgramsTable.Tree where

import Gargantext.Prelude

import Data.Array as A
import Data.Lens ((^..), (^.), view)
import Data.Lens.Fold (folded)
import Data.Lens.Index (ix)
import Data.List (List, intercalate)
import Data.List as L
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (traverse_)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (Variant(..))
import Gargantext.Components.Table as Tbl
import Gargantext.Core.NgramsTable.Functions (applyNgramsPatches, setTermListA, tablePatchHasNgrams)
import Gargantext.Core.NgramsTable.Types (Action(..), CoreAction, NgramsClick, NgramsDepth, NgramsElement, NgramsTable, NgramsTablePatch, NgramsTerm, _NgramsElement, _NgramsRepoElement, _children, _list, _ngrams, _occurrences, ngramsTermText, replace)
import Gargantext.Hooks.FirstEffect (useFirstEffect')
import Gargantext.Types as GT
import Gargantext.Utils ((?))
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
  ( getNgramsChildrenAff :: Maybe (NgramsTerm -> Aff (Array NgramsTerm))
  , getNgramsChildren :: Maybe (NgramsTerm -> Array NgramsTerm)
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
    cpt { getNgramsChildrenAff
        , getNgramsChildren
        , ngramsClick
        , ngramsDepth
        , ngramsEdit
        , ngramsStyle
        } _ = do
      pure $
        H.ul
        { className: "render-ngrams-tree" }
        [ H.span { className: "tree" }
          [ H.span { className: "righthanded" }
            [ tree { getNgramsChildren
                   , getNgramsChildrenAff
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
  ( getNgramsChildrenAff :: Maybe (NgramsTerm -> Aff (Array NgramsTerm))
  , getNgramsChildren :: Maybe (NgramsTerm -> Array NgramsTerm)
  , ngramsEdit           :: NgramsClick
  --, ngramsTable :: NgramsTable
  | TagProps
  )

-- | /!\ Multiple issues to deal in this specific component:
-- |     - stack of patch surgery: monolitic use of the <doctable> +
-- |       design choice of rendering ngrams children on the fly +
-- |       setting up a facade for the `getNgramsChildren` thunk ALWAYS as an
-- |       `Aff` even if not necessary
-- |     - ReactJS re-rendering flaw causing flickering UI effect
-- |     - PureScript pattern matching recursive limitation
-- |
-- |      ↳ workaround: employ a delegation pattern with the an input bearing
-- |        both the `Aff` thunk and a pure one. Note that we could create a
-- |        Typing way, due to the PureScript limitation (see above)
tree :: Record TreeProps -> R.Element
tree p = R.createElement treeCpt p []
treeCpt :: R.Component TreeProps
treeCpt = here.component "tree" cpt where
  cpt props@{ getNgramsChildrenAff
            , getNgramsChildren
            , ngramsDepth
            } _ = do
    -- | States
    -- |
    defaultNgramsChildren <- R.useMemo $ const $
      maybe
        (mempty :: List NgramsTerm)
        (\thunk -> L.fromFoldable $ thunk ngramsDepth.ngrams)
        getNgramsChildren

    ngramsChildren /\ ngramsChildren' <-
      R2.useBox' (defaultNgramsChildren :: List NgramsTerm)

    -- | Hooks
    -- |
    useFirstEffect' $ maybe
      (R.nothing)
      (\aff -> launchAff_ do
        res <- aff ngramsDepth.ngrams
        liftEffect $
          flip T.write_ ngramsChildren' $ L.fromFoldable res
      )
      (getNgramsChildrenAff)

    -- | Render
    -- |
    pure $

      treeLoaded (Record.merge props { ngramsChildren })



type TreeLoaded =
  ( ngramsChildren    :: List NgramsTerm
  | TreeProps )

treeLoaded :: Record TreeLoaded -> R.Element
treeLoaded p = R.createElement treeLoadedCpt p []
treeLoadedCpt :: R.Component TreeLoaded
treeLoadedCpt = here.component "treeLoaded" cpt where
  cpt params@{ ngramsChildren
             , ngramsClick
             , ngramsDepth
             , ngramsEdit
             , ngramsStyle
             } _ = do
    pure $

      H.li
      -- { className: "ngrams-tree-loaded-node" }
      { className: intercalate " "
          [ "ngrams-tree-loaded-node"
          , ngramsDepth.depth == 1 ?
              "ngrams-tree-loaded-node--first-child" $
              ""
          , ngramsDepth.depth > 1 ?
              "ngrams-tree-loaded-node--grand-child" $
              ""
          ]
      }
      (
        -- @NOTE #414: currently commenting this, as the below icon is not
        --             a call-to-action, thus deceiving the user of possible
        --             yet-to-become reveal/collapse node children feature
        -- [ H.i { className, style } [] ]
      -- <>
        [ R2.buff $ tag [ text $ " " <> ngramsTermText ngramsDepth.ngrams ] ]
      <>
        maybe [] edit (ngramsEdit ngramsDepth)
      <>
        [ forest ngramsChildren ]
      )
    where
      tag =
        case ngramsClick ngramsDepth of
          Just effect ->
            a (ngramsStyle <> [DOM.onClick $ const effect])
          Nothing ->
            span ngramsStyle
      edit effect =
        [
          B.iconButton
          { name: "plus-minus"
          , className: "tree-loaded-plus"
          , variant: Secondary
          , callback: const effect
          , overlay: false
          , title: "Combine and separate"
          }
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
  ( dispatch             :: Action -> Effect Unit
  , getNgramsChildrenAff :: Maybe (NgramsTerm -> Aff (Array NgramsTerm))
  , getNgramsChildren :: Maybe (NgramsTerm -> Array NgramsTerm)
  , isEditing         :: T.Box Boolean
  , ngrams            :: NgramsTerm
  , ngramsElement     :: NgramsElement
  , ngramsLocalPatch  :: NgramsTablePatch
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
        , isEditing
        , ngrams
        , ngramsElement
        , ngramsLocalPatch
        , ngramsSelection
        , ngramsTable
        } _ = do
      isEditing' <- T.useLive T.unequal isEditing

      pure $ Tbl.makeRow
        [
          selected
        ,
          B.wad'
          [ "pl-3" ] $
          show $ A.length $ A.fromFoldable (ngramsElement ^. _NgramsElement <<< _occurrences)
        ,
          H.div {}
          ( if isEditing'
            then
              [
                B.iconButton
                { name: "plus"
                , className: "mr-1 align-bottom"
                , overlay: false
                , variant: Primary
                , callback: const $ dispatch $ ToggleChild true ngrams
                }
              ,
                R2.buff $
                tag [ text $ " " <> ngramsTermText ngramsDepth.ngrams ]
              ]
            else
              [
                renderNgramsTree
                { getNgramsChildrenAff: Nothing
                , getNgramsChildren: Just $ getNgramsChildren'
                , ngramsClick
                , ngramsDepth
                , ngramsEdit
                , ngramsStyle
                , key: ""
                }
              ]
          )
      ]
      where
        ngramsDepth = { ngrams, depth: 0 }
        tag =
          case ngramsClick ngramsDepth of
            Just effect ->
              a (ngramsStyle <> [DOM.onClick $ const effect])
            Nothing ->
              span ngramsStyle

        termList :: GT.TermList
        termList    = ngramsElement ^. _NgramsElement <<< _list
        ngramsStyle :: Array DOM.Props
        ngramsStyle = [termStyle termList ngramsOpacity]
        ngramsEdit { ngrams: n } = Just $ dispatch $ SetParentResetChildren (Just n) (ngramsChildren n)
        tbl :: NgramsTable
        tbl = applyNgramsPatches { ngramsLocalPatch
                                 , ngramsStagePatch: mempty
                                 , ngramsValidPatch: mempty
                                 , ngramsVersion: 0 } ngramsTable
        getNgramsChildren' :: NgramsTerm -> Array NgramsTerm
        getNgramsChildren' n = A.fromFoldable $ ngramsChildren n
        ngramsChildren :: NgramsTerm -> List NgramsTerm
        ngramsChildren n = tbl ^.. ix n <<< _NgramsRepoElement <<< _children <<< folded
        ngramsClick :: { depth :: Int, ngrams :: NgramsTerm } -> Maybe (Effect Unit)
        ngramsClick p = Just $ do
          traverse_ (dispatch <<< CoreAction <<< cycleTermListItem) (A.cons p.ngrams $ getNgramsChildren' p.ngrams)
          -- ^ This is the old behavior it is nicer to use since one can
          --   rapidly change the ngram list without waiting for confirmation.
          --   However this might expose bugs. One of them can be reproduced
          --   by clicking a multiple times on the same ngram, sometimes it stays
          --   transient.
          -- | ngramsTransient = const Nothing
          -- | otherwise       = Just <<< dispatch <<< cycleTermListItem <<< view _ngrams
        selected    =
          H.div
          { on: { click: const $ dispatch $ ToggleSelect ngrams
                }
          }
          [
            B.icon
            { name: Set.member ngrams ngramsSelection ?
                "check-square" $
                "square-o"
            , className: Set.member ngrams ngramsSelection ?
                "color-primary" $
                ""
            }
          ]

        -- (?) removing quick action turning ngram to Candidate or Stop via
        --     a checkbox click
        -- checkbox termList' =
        --   let chkd = termList == termList'
        --       termList'' = if chkd then GT.CandidateTerm else termList'
        --   in
        --     B.wad
        --     [ "text-center" ]
        --     [
        --       H.input
        --       { checked: chkd
        --       , className: "checkbox"
        --       , on: { change: const $ dispatch $ CoreAction $
        --               setTermListA ngrams (replace termList termList'') }
        --       , readOnly: ngramsTransient
        --       , type: "checkbox"
        --       , style:
        --           { cursor: "pointer"
        --           , marginTop: "6px"
        --           }
        --       }
        --     ]

        ngramsTransient = tablePatchHasNgrams ngramsLocalPatch ngrams
          -- ^ TODO here we do not look at ngramsNewElems, shall we?
        ngramsOpacity
          | ngramsTransient = 0.5
          | otherwise       = 1.0

        cycleTermListItem :: NgramsTerm -> CoreAction
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
