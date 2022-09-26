module Gargantext.Components.NgramsTable
  ( MainNgramsTableProps
  , CommonProps
  , TreeEdit
  , NgramsTreeEditProps
  , getNgramsChildrenAff
  , initialTreeEdit
  , mainNgramsTable
  ) where

import Gargantext.Prelude

import Data.Array as A
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens (to, view, (.~), (^.), (^?))
import Data.Lens.At (at)
import Data.Lens.Common (_Just)
import Data.Lens.Fold (folded)
import Data.Lens.Index (ix)
import Data.List (List, intercalate)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Ord.Down (Down(..))
import Data.Sequence as Seq
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Gargantext.Components.App.Store (Boxes)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), Sizing(..), Variant(..))
import Gargantext.Components.NgramsTable.Loader (useLoaderWithCacheAPI)
import Gargantext.Components.NgramsTable.Search as NTS
import Gargantext.Components.NgramsTable.SelectionCheckbox as NTSC
import Gargantext.Components.NgramsTable.SyncResetButton (syncResetButtons)
import Gargantext.Components.NgramsTable.Tree (renderNgramsItem, renderNgramsTree)
import Gargantext.Components.Nodes.Lists.Types as NT
import Gargantext.Components.Table as TT
import Gargantext.Components.Table.Types as TT
import Gargantext.Config.REST (AffRESTError, RESTError, logRESTError)
import Gargantext.Core.NgramsTable.Functions (addNewNgramA, applyNgramsPatches, chartsAfterSync, commitPatch, convOrderBy, coreDispatch, filterTermSize, ngramsRepoElementToNgramsElement, normNgram, patchSetFromMap, singletonNgramsTablePatch, tablePatchHasNgrams, toVersioned)
import Gargantext.Core.NgramsTable.Types (Action(..), CoreAction(..), CoreState, Dispatch, NgramsActionRef, NgramsClick, NgramsElement(..), NgramsPatch(..), NgramsTable, NgramsTablePatch(..), NgramsTerm(..), PageParams, PatchMap(..), Versioned(..), VersionedNgramsTable, VersionedWithCountNgramsTable, _NgramsElement, _NgramsRepoElement, _NgramsTable, _children, _list, _ngrams, _ngrams_repo_elements, _ngrams_scores, _occurrences, _root, applyPatchSet, ngramsTermText, replace)
import Gargantext.Hooks.Loader (useLoaderBox)
import Gargantext.Routes (SessionRoute(..)) as Routes
import Gargantext.Sessions (Session, get)
import Gargantext.Types (CTabNgramType, ListId, NodeID, OrderBy(..), SearchQuery, TabType, TermList(..), TermSize, termLists, termSizes)
import Gargantext.Utils (nbsp, queryExactMatchesLabel, queryMatchesLabel, sortWith, toggleSet)
import Gargantext.Utils.CacheAPI as GUC
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Seq as Seq
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Toestand as T
import Unsafe.Coerce (unsafeCoerce)

here :: R2.Here
here = R2.here "Gargantext.Components.NgramsTable"

type TreeEdit =
  { isEditing          :: Boolean
  , ngramsChildren     :: List NgramsTerm
                       -- ^ Root children, as were originally present
                       --   in the table, before editing
  , ngramsChildrenDiff :: Map NgramsTerm Boolean
                       -- ^ Used only when grouping.
                       --   This updates the children of `ngramsParent`,
                       --   ngrams set to `true` are to be added, and `false` to
                       --   be removed.
  , ngramsParent       :: Maybe NgramsTerm -- Nothing means we are not currently grouping terms
  }

type State =
  CoreState (
    ngramsSelection  :: Set NgramsTerm
                     -- ^ The set of selected checkboxes of the first column.
  )

initialTreeEdit :: TreeEdit
initialTreeEdit =
  { isEditing         : false
  , ngramsChildren    : List.Nil
  , ngramsChildrenDiff: Map.empty
  , ngramsParent      : Nothing }

initialState :: State
initialState =
  { ngramsLocalPatch: mempty
  , ngramsSelection:  mempty
  , ngramsStagePatch: mempty
  , ngramsValidPatch: mempty
  , ngramsVersion:    0
  }

initialStateWithVersion :: VersionedNgramsTable -> State
initialStateWithVersion (Versioned { version }) = initialState { ngramsVersion = version }

setTermListSetA :: NgramsTable -> Set NgramsTerm -> TermList -> Action
setTermListSetA ngramsTable ns new_list =
  CoreAction $ CommitPatch $ NgramsTablePatch $ PatchMap $ mapWithIndex f $ toMap ns
  where
    f :: NgramsTerm -> Unit -> NgramsPatch
    f n _unit = NgramsPatch { patch_list, patch_children: mempty }
      where
        cur_list = ngramsTable ^? at n <<< _Just <<< _NgramsRepoElement <<< _list
        --patch_list = maybe mempty (replace new_list) cur_list
        patch_list = case cur_list of
          Nothing -> mempty
          Just cl -> replace cl new_list
    toMap :: forall a. Set a -> Map a Unit
    toMap = unsafeCoerce
    -- TODO https://github.com/purescript/purescript-ordered-collections/pull/21
    --      https://github.com/purescript/purescript-ordered-collections/pull/31
    -- toMap = Map.fromFoldable

type PreConversionRows = Seq.Seq NgramsElement

type TableContainerProps =
  ( addCallback       :: String -> Effect Unit
  , dispatch          :: Dispatch
  , getNgramsChildren :: NgramsTerm -> Aff (Array NgramsTerm)
  , ngramsSelection   :: Set NgramsTerm
  , ngramsTable       :: NgramsTable
  , path              :: T.Box PageParams
  , queryExactMatches :: Boolean
  , syncResetButton   :: Array R.Element
  , tabNgramType      :: CTabNgramType
  )

tableContainer :: Record TableContainerProps -> Record TT.TableContainerProps -> R.Element
tableContainer p q = R.createElement (tableContainerCpt p) q []
tableContainerCpt :: Record TableContainerProps -> R.Component TT.TableContainerProps
tableContainerCpt { addCallback
                  , dispatch
                  , getNgramsChildren
                  , ngramsSelection
                  , ngramsTable: ngramsTableCache
                  , path
                  , queryExactMatches
                  , syncResetButton
                  , tabNgramType
                  } = here.component "tableContainer" cpt where
  cpt props _ = do
    -- | States
    -- |
    { searchQuery
    , termListFilter
    , termSizeFilter
    , params
    } <- T.useLive T.unequal path

    -- | Computed
    -- |
    let
      showAddNewTerm =
        (
          (not queryExactMatches || A.null props.tableBody)
        &&
          (searchQuery /= "")
        )

    -- | Render
    -- |
    pure $

      H.div
      { className: intercalate " "
        [ "ngrams-table-container"
        , "card"
        ]
      }
      [

        H.div
        { className: intercalate " "
            [ "ngrams-table-container__header"
            , "card-header"
            ]
        }
        [
        --   H.div
        --   { className: "col-md-2"
        --   , style: { marginTop: "6px" }
        --   }
        --   [
        --     H.li
        --     { className: "list-group-item" }
        --     syncResetButton
        --   ,
        --     -- , if (not $ Set.member (normNgram tabNgramType searchQuery) ngramsSelection) && searchQuery /= "" then
        --
        --   ]
        -- ,

          H.div
          { className: intercalate " "
              [ "ngrams-table-container__header__item"
              , "card"
              ]
          }
          syncResetButton
        ,
          H.div
          { className: intercalate " "
              [ "ngrams-table-container__header__item"
              , "card"
              ]
          }
          [
            R2.select
            { id: "picklistmenu"
            , className: "form-control custom-select"
            , defaultValue: (maybe "" show termListFilter)
            , on: {change: setTermListFilter <<< read <<< R.unsafeEventValue}
            }
            (map optps1 termLists)
          ]
        ,
          H.div
          { className: intercalate " "
              [ "ngrams-table-container__header__item"
              , "card"
              ]
          }
          [
            R2.select
            { id: "picktermtype"
            , className: "form-control custom-select"
            , defaultValue: (maybe "" show termSizeFilter)
            , on: {change: setTermSizeFilter <<< read <<< R.unsafeEventValue}
            }
            (map optps1 termSizes)
          ]
        ,
          H.div
          { className: intercalate " "
              [ "ngrams-table-container__header__item"
              , "card"
              ]
          }
          [
            B.wad
            [ "d-flex", "align-items-center" ]
            [
              props.pageSizeControl
            ,
              B.wad_ [ "mr-2", "d-inline-block" ]
            ,
              B.label_ "items"
              --   H.div { className: "col-md-6" } [ props.pageSizeControl ]
              -- , H.div { className: "col-md-6" } [
              --    ]
            ]
          ]
        ,
          H.div
          { className: intercalate " "
              [ "ngrams-table-container__header__item"
              , "card"
              , "flex-grow-1"
              ]
          }
          [
            props.pageSizeDescription
          ,
            props.paginationLinks
          ]
        ]
      ,
        R2.when (selectionsExist ngramsSelection) $

          H.li
          { className: "card" }
          [
            selectButtons true
          ]
      ,
        H.div
        { id: "terms_table"
        , className: "card-body"
        }
        [
          R2.when showAddNewTerm $

            H.div
            { className: "ngrams-table-container__add-term" }
            [
              B.button
              { variant: ButtonVariant Light
              , callback: const $ addCallback searchQuery
              }
              [
                B.icon
                { name: "circle"
                , className: "mr-1 graph-term"
                }
              ,
                H.text "Add"
              ,
                H.text $ nbsp 1
              ,
                B.b_ $ "« " <> searchQuery <> " »"
              ,
                H.text $ nbsp 1
              ,
                H.text "to Map terms"
              ]
            ]
        ,
          H.table
          { className: "table able" }
          [
            H.thead
            {}
            [
              props.tableHead
            ]
          ,
            H.tbody
            {}
            props.tableBody
          ]
        ,
          H.li
          { className: intercalate " "
              [ "ngrams-table-container__footer"
              , "card"
              ]
          }
          [
            H.div
            { className: "ngrams-table-container__footer__item" }
            [
              selectButtons (selectionsExist ngramsSelection)
            ]
          ,
            H.div
            { className: "ngrams-table-container__footer__item" }
            [
              props.paginationLinks
            ]
          ]
        ]
      ]

  -- WHY setPath     f = origSetPageParams (const $ f path)
  setTermListFilter x = T.modify (_ { termListFilter = x }) path
  setTermSizeFilter x = T.modify (_ { termSizeFilter = x }) path
  setSelection term = dispatch $ setTermListSetA ngramsTableCache ngramsSelection term

  selectionsExist :: Set NgramsTerm -> Boolean
  selectionsExist = not <<< Set.isEmpty

  selectButtons false = H.div {} []
  selectButtons true =
    H.div {} [
      H.button { className: "btn btn-primary"
              , on: { click: const $ setSelection MapTerm }
              } [ H.text "Map" ]
      , H.button { className: "btn btn-primary"
                , on: { click: const $ setSelection StopTerm }
                } [ H.text "Stop" ]
      , H.button { className: "btn btn-primary"
                , on: { click: const $ setSelection CandidateTerm }
                } [ H.text "Candidate" ]
    ]

-- NEXT

type CommonProps =
  ( afterSync         :: Unit -> Aff Unit
  , boxes             :: Boxes
  , tabNgramType      :: CTabNgramType
  , withAutoUpdate    :: Boolean -- (?) not used
  )

type PropsNoReload =
  ( cacheState        :: NT.CacheState
  , mTotalRows        :: Maybe Int
  , path              :: T.Box PageParams
  , state             :: T.Box State
  , treeEdit          :: Record NgramsTreeEditProps
  , versioned         :: VersionedNgramsTable
  | CommonProps
  )

type Props =
  ( reloadForest   :: T2.ReloadS
  , reloadRoot     :: T2.ReloadS
  | PropsNoReload )

type LoadedNgramsTableHeaderProps =
  ( searchQuery :: T.Box SearchQuery )

loadedNgramsTableHeader :: R2.Leaf LoadedNgramsTableHeaderProps
loadedNgramsTableHeader = R2.leaf loadedNgramsTableHeaderCpt
loadedNgramsTableHeaderCpt :: R.Component LoadedNgramsTableHeaderProps
loadedNgramsTableHeaderCpt = here.component "loadedNgramsTableHeader" cpt where
  cpt { searchQuery } _ = pure $

    R.fragment
    [
      H.div
      { className: "loaded-ngrams-table-header" }
      [
        B.icon
        { name: "hand-o-down"
        , className: "loaded-ngrams-table-header__icon"
        }
      ,
        B.wad_ [ "mr-1", "d-inline-block" ]
      ,
        B.span'
        { className: "loaded-ngrams-table-header__text" } $
        "Extracted Terms"
      ]
    ,
      NTS.searchInput
      { key: "search-input"
      , searchQuery
      }
    ]

loadedNgramsTableBody :: R2.Component PropsNoReload
loadedNgramsTableBody = R.createElement loadedNgramsTableBodyCpt
loadedNgramsTableBodyCpt :: R.Component PropsNoReload
loadedNgramsTableBodyCpt = here.component "loadedNgramsTableBody" cpt where
  cpt { afterSync
      , boxes: { errors
               , tasks }
      , cacheState
      , mTotalRows
      , path
      , state
      , tabNgramType
      , treeEdit: treeEdit@{ getNgramsChildren }
      , versioned: Versioned { data: initTable }
      } _ = do
    treeEdit'@{ ngramsParent } <- T.useLive T.unequal treeEdit.box
    state'@{ ngramsLocalPatch, ngramsSelection } <- T.useLive T.unequal state
    path'@{ scoreType, termListFilter, termSizeFilter } <- T.useLive T.unequal path
    params <- T.useFocused (_.params) (\a b -> b { params = a }) path
    params'@{ orderBy } <- T.useLive T.unequal params
    searchQueryFocused <- T.useFocused (_.searchQuery) (\a b -> b { searchQuery = a }) path
    searchQuery <- T.useLive T.unequal searchQueryFocused
    isEditing <- T.useFocused (_.isEditing) (\a b -> b { isEditing = a}) treeEdit.box

    let ngramsTable = applyNgramsPatches state' initTable
        rowMap (Tuple ng nre) =
          let ng_scores :: Map NgramsTerm (Additive Int)
              ng_scores = ngramsTable ^. _NgramsTable <<< _ngrams_scores
              Additive s = ng_scores ^. at ng <<< _Just
              addOcc ne =
                let Additive occurrences = sumOccurrences ngramsTable (ngramsElementToNgramsOcc ne) in
                ne # _NgramsElement <<< _occurrences .~ occurrences
          in
          addOcc <$> rowsFilter (ngramsRepoElementToNgramsElement ng s nre)
        rows :: PreConversionRows
        rows = ngramsTableOrderWith orderBy (Seq.mapMaybe rowMap nres)
        nres = Map.toUnfoldable (ngramsTable ^. _NgramsTable <<< _ngrams_repo_elements)
        rootOfMatch (Tuple ng nre) =
          if queryMatchesLabel searchQuery (ngramsTermText ng)
          then Just (fromMaybe ng (nre ^. _NgramsRepoElement <<< _root))
          else Nothing
        rootsWithMatches = Set.fromFoldable (Seq.mapMaybe rootOfMatch nres)
        exactMatches :: Boolean
        exactMatches = not $ Seq.null $ Seq.filter fltr nres
          where
            fltr (Tuple ng _) = queryExactMatchesLabel searchQuery (ngramsTermText ng)
        rowsFilter :: NgramsElement -> Maybe NgramsElement
        rowsFilter ngramsElement =
          if displayRow { ngramsElement
                        , ngramsParentRoot
                        , rootsWithMatches
                        , state: state'
                        , termListFilter
                        , termSizeFilter
                        , treeEdit: treeEdit' } then
            Just ngramsElement
          else
            Nothing

        performAction = mkDispatch { filteredRows
                                   , path: path'
                                   , state
                                   , treeEdit }

        -- filteredRows :: PreConversionRows
        -- no need to filter offset if cache is off
        filteredRows = if cacheState == NT.CacheOn then TT.filterRows { params: params' } rows else rows
        filteredConvertedRows :: TT.Rows
        filteredConvertedRows = convertRow <$> filteredRows

        convertRow ngramsElement =
          { row: renderNgramsItem { dispatch: performAction
                                  , getNgramsChildren
                                  , isEditing
                                  , ngrams: ngramsElement ^. _NgramsElement <<< _ngrams
                                  , ngramsElement
                                  , ngramsLocalPatch
                                  , ngramsSelection
                                  , ngramsTable } []
          , delete: false
          }

        allNgramsSelected = allNgramsSelectedOnFirstPage ngramsSelection filteredRows

        totalRecords = fromMaybe (Seq.length rows) mTotalRows

        afterSync' _ = do
          chartsAfterSync path' errors tasks unit
          afterSync unit

        syncResetButton = syncResetButtons { afterSync: afterSync'
                                           , ngramsLocalPatch
                                           , performAction: performAction <<< CoreAction }

        addCallback searchQuery = do
          -- add new ngram as a "Map Term"
          performAction
            $ CoreAction
            $ addNewNgramA (normNgram tabNgramType searchQuery) MapTerm
          -- then sync the ngram list
          performAction
            $ CoreAction
            $ Synchronize { afterSync: afterSync' }


        -- autoUpdate :: Array R.Element
--         autoUpdate = if withAutoUpdate then
--                        [ R2.buff
--                        $ autoUpdateElt
--                          { duration: 5000
--                          , effect: performAction $ CoreAction $ Synchronize { afterSync: afterSync' }
--                          }
--                        ]
--                      else []

        ngramsParentRoot :: Maybe NgramsTerm
        ngramsParentRoot =
          (\np -> ngramsTable ^? at np
                            <<< _Just
                            <<< _NgramsRepoElement
                            <<< _root
                            <<< _Just
            ) =<< ngramsParent

    R.useEffect' $ do
      R.setRef treeEdit.onCancelRef $ Just $ const $ performAction ClearTreeEdit
      R.setRef treeEdit.onSaveRef $ Just $ const $ performAction AddTermChildren
      let ngramsClick { depth: 1, ngrams: child } = Just $ performAction $ ToggleChild  false child
          ngramsClick  _ = Nothing
      R.setRef treeEdit.onNgramsClickRef $ Just ngramsClick

    pure $ R.fragment
      [ TT.table
        { colNames
        , container: tableContainer
          { addCallback
          , dispatch: performAction
          , getNgramsChildren
          , ngramsSelection
          , ngramsTable
          , path
          , queryExactMatches: exactMatches
          , syncResetButton: [ syncResetButton ]
          , tabNgramType
          }
        , params
        , rows: filteredConvertedRows
        , syncResetButton: [ syncResetButton ]
        , totalRecords
        , wrapColElts:
          wrapColElts { allNgramsSelected, dispatch: performAction, ngramsSelection } scoreType
        }
      ,
        B.wad
        [ "mt-2", "d-inline-block" ]
        [
          syncResetButton
        ]
      ]
      where
        colNames = TT.ColumnName <$> ["Show", "Select", "Map", "Stop", "Terms", "Score"] -- see convOrderBy

ngramsTableOrderWith orderBy =
  case convOrderBy <$> orderBy of
    Just ScoreAsc  -> sortWith \x -> x        ^. _NgramsElement <<< _occurrences
    Just ScoreDesc -> sortWith \x -> Down $ x ^. _NgramsElement <<< _occurrences
    Just TermAsc   -> sortWith \x -> x        ^. _NgramsElement <<< _ngrams
    Just TermDesc  -> sortWith \x -> Down $ x ^. _NgramsElement <<< _ngrams
    _              -> identity -- the server ordering is enough here

-- This is used to *decorate* the Select header with the checkbox.
wrapColElts scProps _         (TT.ColumnName "Select") = const [NTSC.selectionCheckbox scProps]
wrapColElts _       scoreType (TT.ColumnName "Score")  = (_ <> [H.text ("(" <> show scoreType <> ")")])
wrapColElts _       _         _                        = identity

type MkDispatchProps = (
    filteredRows :: PreConversionRows
  , path         :: PageParams
  , state        :: T.Box State
  , treeEdit     :: Record NgramsTreeEditProps
  )

mkDispatch :: Record MkDispatchProps -> (Action -> Effect Unit)
mkDispatch { filteredRows
           , path
           , state
           , treeEdit } = performAction
  where
    performAction :: Action -> Effect Unit
    performAction ClearTreeEdit = do
      T.write_ initialTreeEdit treeEdit.box
    performAction (SetParentResetChildren ngramsParent ngramsChildren) = do
      T.write_ { isEditing: true
               , ngramsChildren
               , ngramsChildrenDiff: Map.empty
               , ngramsParent } treeEdit.box
    performAction (ToggleChild b c) = do
      T.modify_ (\g@{ ngramsChildrenDiff: ncd } -> g { ngramsChildrenDiff = newNC ncd }) treeEdit.box
      where
        newNC ncd = Map.alter (maybe (Just b) (const Nothing)) c ncd
    performAction (ToggleSelect c) =
      T.modify_ (\s@{ ngramsSelection: ns } -> s { ngramsSelection = toggleSet c ns }) state
    performAction ToggleSelectAll = do
      { ngramsSelection } <- T.read state
      T.modify_ (toggler ngramsSelection) state
      where
        toggler ngramsSelection s =
          if allNgramsSelectedOnFirstPage ngramsSelection filteredRows then
            s { ngramsSelection = Set.empty :: Set NgramsTerm }
          else
            s { ngramsSelection = selectNgramsOnFirstPage filteredRows }
    performAction AddTermChildren = do
      { ngramsChildren, ngramsChildrenDiff, ngramsParent } <- T.read treeEdit.box
      case ngramsParent of
        Nothing ->
          -- impossible but harmless
          pure unit
        Just parent -> do
          let pc = patchSetFromMap ngramsChildrenDiff
              pe = NgramsPatch { patch_list: mempty, patch_children: pc }
              pt = singletonNgramsTablePatch parent pe
          performAction ClearTreeEdit
          -- let ppt = case (A.head $ Set.toUnfoldable $ Map.keys ngramsChildrenDiff) of
          --       Nothing -> mempty
          --       Just h  ->
          --         let pp = NgramsPatch { patch_list: mempty
          --                              , patch_children: patchSetFromMap $ Map.mapMaybe (\v -> Just $ not v) ngramsChildrenDiff }
          --         in
          --         singletonNgramsTablePatch h pp
          -- here.log2 "[performAction] pt with patchSetFromMap" $ pt <> ppt
          commitPatch (pt {-<> ppt-}) state
    performAction (CoreAction a) = coreDispatch path state a


displayRow :: { ngramsElement    :: NgramsElement
              , ngramsParentRoot :: Maybe NgramsTerm
              , rootsWithMatches :: Set NgramsTerm
              , state            :: State
              , termListFilter   :: Maybe TermList
              , termSizeFilter   :: Maybe TermSize
              , treeEdit         :: TreeEdit } -> Boolean
displayRow { ngramsElement: NgramsElement {ngrams, root, list}
           , ngramsParentRoot
           , state: { ngramsLocalPatch }
           , rootsWithMatches
           , termListFilter
           , termSizeFilter
           , treeEdit: { ngramsChildren
                       , ngramsChildrenDiff
                       , ngramsParent } } =
    -- See these issues about the evolution of this filtering.
    -- * https://gitlab.iscpif.fr/gargantext/purescript-gargantext/issues/340
    -- * https://gitlab.iscpif.fr/gargantext/haskell-gargantext/issues/87
       isNothing root
    -- ^ Display only nodes without parents.
    && Set.member ngrams rootsWithMatches
    -- ^ and which matches the search query.
    && maybe true (_ == list) termListFilter
    -- ^ and which matches the ListType filter.
    && ngramsChildrenDiff ^. at ngrams /= Just true
    -- ^ and which are not scheduled to be added already
    && Just ngrams /= ngramsParent
    -- ^ and which are not our new parent
    && Just ngrams /= ngramsParentRoot
    -- ^ and which are not the root of our new parent
    && filterTermSize termSizeFilter ngrams
    -- ^ and which satisfies the chosen term size
    || ngramsChildrenDiff ^. at ngrams == Just false
    -- ^ unless they are scheduled to be removed.
    || tablePatchHasNgrams ngramsLocalPatch ngrams
    -- ^ unless they are being processed at the moment.

allNgramsSelectedOnFirstPage :: Set NgramsTerm -> PreConversionRows -> Boolean
allNgramsSelectedOnFirstPage selected rows = selected == (selectNgramsOnFirstPage rows)

selectNgramsOnFirstPage :: PreConversionRows -> Set NgramsTerm
selectNgramsOnFirstPage rows = Set.fromFoldable $ (view $ _NgramsElement <<< _ngrams) <$> rows


type MainNgramsTableProps = (
    cacheState    :: T.Box NT.CacheState
  , defaultListId :: Int
    -- ^ This node can be a corpus or contact.
  , path          :: T.Box PageParams
  , session       :: Session
  , tabType       :: TabType
  , treeEdit      :: Record NgramsTreeEditProps
  | CommonProps
  )

getNgramsChildrenAff :: Session -> NodeID -> Array ListId -> TabType -> NgramsTerm -> Aff (Array NgramsTerm)
getNgramsChildrenAff session nodeId listIds tabType (NormNgramsTerm ngrams) = do
  res :: Either RESTError ({ data :: Array { children :: Array String, ngrams :: String }}) <- get session $ Routes.GetNgrams params (Just nodeId)
  case res of
    Left err -> pure []
    Right { data: lst } -> case A.uncons (A.filter (\d -> d.ngrams == ngrams) lst) of
      Nothing -> pure []
      Just { head } -> pure $ NormNgramsTerm <$> head.children
  where
    params = { limit: 10
             , listIds
             , offset: Nothing
             , orderBy: Nothing
             , searchQuery: ngrams
             , tabType
             , termListFilter: Nothing
             , termSizeFilter: Nothing }

mainNgramsTable :: R2.Component MainNgramsTableProps
mainNgramsTable = R.createElement mainNgramsTableCpt
mainNgramsTableCpt :: R.Component MainNgramsTableProps
mainNgramsTableCpt = here.component "mainNgramsTable" cpt
  where
    cpt props@{ cacheState, path, treeEdit } _ = do
      searchQuery <- T.useFocused (_.searchQuery) (\a b -> b { searchQuery = a }) path
      cacheState' <- T.useLive T.unequal cacheState
      -- onCancelRef <- R.useRef Nothing
      -- onNgramsClickRef <- R.useRef Nothing
      -- onSaveRef   <- R.useRef Nothing
      state <- T.useBox initialState
      -- ngramsLocalPatch <- T.useFocused (_.ngramsLocalPatch) (\a b -> b { ngramsLocalPatch = a }) state

      -- nodeId <- T.useFocused (_.nodeId) (\a b -> b { nodeId = a }) path
      -- nodeId' <- T.useLive T.unequal nodeId

      -- let treeEdit = { box: treeEditBox
      --                , getNgramsChildren: getNgramsChildrenAff session nodeId' tabType
      --                , onCancelRef
      --                , onNgramsClickRef
      --                , onSaveRef
      --                }

      -- let path = initialPageParams session nodeId [defaultListId] tabType

      case cacheState' of
        NT.CacheOn  -> pure $ R.fragment
          [ loadedNgramsTableHeader { searchQuery }
          , ngramsTreeEdit (treeEdit)
          , mainNgramsTableCacheOn (Record.merge props { state })
          ]
        NT.CacheOff -> pure $ R.fragment
          [loadedNgramsTableHeader { searchQuery }
          , ngramsTreeEdit (treeEdit)
          , mainNgramsTableCacheOff (Record.merge props { state })
          ]


type NgramsTreeEditProps =
  ( box               :: T.Box TreeEdit
  , getNgramsChildren :: NgramsTerm -> Aff (Array NgramsTerm)
  --, ngramsLocalPatch  :: T.Box NgramsTablePatch
  , onCancelRef       :: NgramsActionRef
  , onNgramsClickRef  :: R.Ref (Maybe NgramsClick)
  , onSaveRef         :: NgramsActionRef
  )

ngramsTreeEdit :: R2.Leaf NgramsTreeEditProps
ngramsTreeEdit = R2.leaf ngramsTreeEditCpt
ngramsTreeEditCpt :: R.Component NgramsTreeEditProps
ngramsTreeEditCpt = here.component "ngramsTreeEdit" cpt where
  cpt props@{ box } _ = do
    isEditingFocused <- T.useFocused (_.isEditing) (\a b -> b { isEditing = a }) box
    isEditingFocused' <- T.useLive T.unequal isEditingFocused
    ngramsParentFocused <- T.useFocused (_.ngramsParent) (\a b -> b { ngramsParent = a}) box
    ngramsParentFocused' <- T.useLive T.unequal ngramsParentFocused

    let
      gutter = B.wad_ [ "mb-2", "d-inline-block" ]

    pure $ if isEditingFocused'
      then case ngramsParentFocused' of
                Nothing -> gutter
                Just ngramsParent' -> ngramsTreeEditReal (Record.merge props { ngramsParent' })
      else gutter

type NgramsTreeEditRealProps =
  ( ngramsParent' :: NgramsTerm
  | NgramsTreeEditProps )

ngramsTreeEditReal :: R2.Leaf NgramsTreeEditRealProps
ngramsTreeEditReal = R2.leaf ngramsTreeEditRealCpt
ngramsTreeEditRealCpt :: R.Component NgramsTreeEditRealProps
ngramsTreeEditRealCpt = here.component "ngramsTreeEditReal" cpt where
  cpt { box
      , getNgramsChildren
      , ngramsParent'
      , onCancelRef
      , onNgramsClickRef
      , onSaveRef
      } _ = do
    -- | States
    -- |
    { ngramsChildren
    , ngramsChildrenDiff
    } <- T.useLive T.unequal box

    -- | Computed
    -- |
    let
      ngramsDepth = { depth: 0, ngrams: ngramsParent' }

      ngramsChildrenPatched :: Set NgramsTerm
      ngramsChildrenPatched = applyPatchSet (patchSetFromMap ngramsChildrenDiff) $ Set.fromFoldable ngramsChildren
      -- A patched version of getNgramsChildren. This is used
      -- because we're editing the tree and so won't fetch the API
      -- ngrams children.
      gnc ngrams =
        if ngrams == ngramsParent'
        then do
          pure $ A.fromFoldable ngramsChildrenPatched
        else do
          pure []


    -- | Render
    -- |
    pure $

      H.div
      { className: intercalate " "
          [ "ngrams-tree-edit-real"
          , "card"
          ]
      }
      [
        H.div
        { className: "card-header" }
        [
          B.icon
          { name: "pencil-square-o"
          }
        ,
          B.wad_
          [ "mr-1", "d-inline-block" ]
        ,
          B.b_ $ ngramsTermText ngramsDepth.ngrams
        ]
      ,
        H.div
        { className: "card-body" }
        [
          renderNgramsTree
          { getNgramsChildren: gnc
          , ngramsClick
          , ngramsDepth
          , ngramsEdit
          , ngramsStyle: []
          , key: show ngramsParent'
                  <> "-" <> show ngramsChildren
                  <> "-" <> show ngramsChildrenDiff
          }
        ,
          H.div
          { className: "ngrams-tree-edit-real__actions" }
          [
            B.button
            { variant: ButtonVariant Light
            , callback: onCancelClick --(const $ dispatch ClearTreeEdit)}
            , size: SmallSize
            }
            [ H.text "Cancel" ]
          ,
            B.button
            { variant: ButtonVariant Primary
            , callback: onSaveClick --(const $ dispatch AddTermChildren)}
            , size: SmallSize
            }
            [ H.text "Save" ]
          ]
        ]
      ]
      -- | Helpers
      -- |
      where
        --ngramsClick {depth: 1, ngrams: child} = Just $ dispatch $ ToggleChild false child
        --ngramsClick _ = Nothing
        ngramsClick :: NgramsClick
        ngramsClick nd = case R.readRef onNgramsClickRef of
          Nothing  -> Nothing
          Just ngc -> ngc nd
        ngramsEdit :: NgramsClick
        ngramsEdit  _ = Nothing
        onCancelClick :: Unit -> Effect Unit
        onCancelClick _ = case R.readRef onCancelRef of
          Nothing -> pure unit
          Just onCancel -> onCancel unit
        onSaveClick :: Unit -> Effect Unit
        onSaveClick _ = case R.readRef onSaveRef of
          Nothing -> pure unit
          Just onSave -> onSave unit

type MainNgramsTableCacheProps =
  ( state :: T.Box State
  | MainNgramsTableProps )

mainNgramsTableCacheOn :: R2.Leaf MainNgramsTableCacheProps
mainNgramsTableCacheOn = R2.leaf mainNgramsTableCacheOnCpt
mainNgramsTableCacheOnCpt :: R.Component MainNgramsTableCacheProps
mainNgramsTableCacheOnCpt = here.component "mainNgramsTableCacheOn" cpt where
  cpt { afterSync
      , boxes
      , defaultListId
      , path
      , state
      , tabNgramType
      , treeEdit
      , withAutoUpdate } _ = do

    -- let path = initialPageParams session nodeId [defaultListId] tabType

    path' <- T.useLive T.unequal path
    let render versioned = mainNgramsTablePaint { afterSync
                                                , boxes
                                                , cacheState: NT.CacheOn
                                                , path
                                                , state
                                                , tabNgramType
                                                , treeEdit
                                                , versioned
                                                , withAutoUpdate } []
    useLoaderWithCacheAPI {
        cacheEndpoint: versionEndpoint { defaultListId, path: path' }
      , errorHandler
      , handleResponse
      , mkRequest
      , path: path'
      , renderer: render
      , spinnerClass: Nothing
      }
  versionEndpoint { defaultListId, path: { nodeId, tabType, session } } _ = get session $ Routes.GetNgramsTableVersion { listId: defaultListId, tabType } (Just nodeId)
  errorHandler = logRESTError here "[mainNgramsTableCacheOn]"
  mkRequest :: PageParams -> GUC.Request
  mkRequest path@{ session } = GUC.makeGetRequest session $ url path
    where
      url { listIds
          , nodeId
          , tabType
          } = Routes.GetNgramsTableAll { listIds
                                  , tabType } (Just nodeId)
  handleResponse :: VersionedNgramsTable -> VersionedNgramsTable
  handleResponse v = v

mainNgramsTableCacheOff :: R2.Leaf MainNgramsTableCacheProps
mainNgramsTableCacheOff = R2.leaf mainNgramsTableCacheOffCpt
mainNgramsTableCacheOffCpt :: R.Component MainNgramsTableCacheProps
mainNgramsTableCacheOffCpt = here.component "mainNgramsTableCacheOff" cpt where
  cpt { afterSync
      , boxes
      , path
      , state
      , tabNgramType
      , treeEdit
      , withAutoUpdate } _ = do
    let render versionedWithCount = mainNgramsTablePaintNoCache { afterSync
                                                                , boxes
                                                                , cacheState: NT.CacheOff
                                                                , path
                                                                , state
                                                                , tabNgramType
                                                                , treeEdit
                                                                , versionedWithCount
                                                                , withAutoUpdate } []
    useLoaderBox { errorHandler
                 , loader
                 , path
                 , render }

  errorHandler = logRESTError here "[mainNgramsTableCacheOff]"

  -- NOTE With cache off
  loader :: PageParams -> AffRESTError VersionedWithCountNgramsTable
  loader { listIds
         , nodeId
         , params: { limit, offset }
         , searchQuery
         , session
         , tabType
         , termListFilter
         , termSizeFilter
         } =
    get session $ Routes.GetNgrams params (Just nodeId)
    where
      params = { limit
               , listIds
               , offset: Just offset
               , orderBy: Nothing  -- TODO
               , searchQuery
               , tabType
               , termListFilter
               , termSizeFilter
               }


type MainNgramsTablePaintProps = (
    cacheState        :: NT.CacheState
  , path              :: T.Box PageParams
  , state             :: T.Box State
  , treeEdit          :: Record NgramsTreeEditProps
  , versioned         :: VersionedNgramsTable
  | CommonProps
  )

mainNgramsTablePaint :: R2.Component MainNgramsTablePaintProps
mainNgramsTablePaint = R.createElement mainNgramsTablePaintCpt
mainNgramsTablePaintCpt :: R.Component MainNgramsTablePaintProps
mainNgramsTablePaintCpt = here.component "mainNgramsTablePaint" cpt
  where
    cpt { afterSync
        , boxes
        , cacheState
        , path
        , state
        , tabNgramType
        , treeEdit
        , versioned
        , withAutoUpdate } _ = do
      R.useEffectOnce' $ do
        let (Versioned { version }) = versioned
        T.modify_ (_ { ngramsVersion = version }) state

      pure $
        loadedNgramsTableBody
        { afterSync
        , boxes
        , cacheState
        , mTotalRows: Nothing
        , path
        , state
        , tabNgramType
        , treeEdit
        , versioned
        , withAutoUpdate
        } []

type MainNgramsTablePaintNoCacheProps = (
    cacheState         :: NT.CacheState
  , path               :: T.Box PageParams
  , state              :: T.Box State
  , treeEdit           :: Record NgramsTreeEditProps
  , versionedWithCount :: VersionedWithCountNgramsTable
  | CommonProps
  )

mainNgramsTablePaintNoCache :: R2.Component MainNgramsTablePaintNoCacheProps
mainNgramsTablePaintNoCache = R.createElement mainNgramsTablePaintNoCacheCpt
mainNgramsTablePaintNoCacheCpt :: R.Component MainNgramsTablePaintNoCacheProps
mainNgramsTablePaintNoCacheCpt = here.component "mainNgramsTablePaintNoCache" cpt
  where
    cpt { afterSync
        , boxes
        , cacheState
        , path
        , state
        , tabNgramType
        , treeEdit
        , versionedWithCount
        , withAutoUpdate } _ = do
      -- TODO This is lame, make versionedWithCount a proper box?
      let count /\ versioned = toVersioned versionedWithCount

      R.useEffectOnce' $ do
        let (Versioned { version }) = versioned
        T.modify_ (_ { ngramsVersion = version }) state

      pure $
        loadedNgramsTableBody
        { afterSync
        , boxes
        , cacheState
        , mTotalRows: Just count
        , path
        , state
        , tabNgramType
        , treeEdit
        , versioned
        , withAutoUpdate } []

type NgramsOcc = { occurrences :: Additive Int, children :: Set NgramsTerm }

ngramsElementToNgramsOcc :: NgramsElement -> NgramsOcc
ngramsElementToNgramsOcc (NgramsElement {occurrences, children}) = {occurrences: Additive occurrences, children}

sumOccurrences :: NgramsTable -> NgramsOcc -> Additive Int
sumOccurrences nt = sumOccChildren mempty
    where
      sumOccTerm :: Set NgramsTerm -> NgramsTerm -> Additive Int
      sumOccTerm seen label
        | Set.member label seen = Additive 0 -- TODO: Should not happen, emit a warning/error.
        | otherwise =
            sumOccChildren (Set.insert label seen)
                           { occurrences: nt ^. _NgramsTable <<< _ngrams_scores <<< ix label
                           , children:    nt ^. ix label <<< _NgramsRepoElement <<< _children
                           }
      sumOccChildren :: Set NgramsTerm -> NgramsOcc -> Additive Int
      sumOccChildren seen {occurrences, children} =
        occurrences <> children ^. folded <<< to (sumOccTerm seen)

optps1 :: forall a. Show a => { desc :: String, mval :: Maybe a } -> R.Element
optps1 { desc, mval } = H.option { value: value } [H.text desc]
  where value = maybe "" show mval
