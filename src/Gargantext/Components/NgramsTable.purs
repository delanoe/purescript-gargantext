module Gargantext.Components.NgramsTable
  ( MainNgramsTableProps
  , CommonProps
  , mainNgramsTable
  ) where

import Data.Array as A
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens (Lens', to, (%~), (.~), (^.), (^?), view)
import Data.Lens.At (at)
import Data.Lens.Common (_Just)
import Data.Lens.Fold (folded)
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Ord.Down (Down(..))
import Data.Sequence (Seq, length) as Seq
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Reactix (Component, Element, Ref, State, createElement, fragment, hooksComponentWithModule, unsafeEventValue, useState') as R
import Reactix.DOM.HTML as H
import Toestand as T

import Unsafe.Coerce (unsafeCoerce)

import Gargantext.Prelude

import Gargantext.AsyncTasks as GAT
import Gargantext.Components.AutoUpdate (autoUpdateElt)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Components.Table as T
import Gargantext.Components.Table.Types as T
import Gargantext.Components.NgramsTable.Components as NTC
import Gargantext.Components.NgramsTable.Core (Action(..), CoreAction(..), CoreState, Dispatch, NgramsElement(..), NgramsPatch(..), NgramsTable, NgramsTerm, PageParams, PatchMap(..), Version, Versioned(..), VersionedNgramsTable, VersionedWithCountNgramsTable, _NgramsElement, _NgramsRepoElement, _NgramsTable, _children, _list, _ngrams, _ngrams_repo_elements, _ngrams_scores, _occurrences, _root, addNewNgramA, applyNgramsPatches, applyPatchSet, chartsAfterSync, commitPatch, convOrderBy, coreDispatch, filterTermSize, fromNgramsPatches, ngramsRepoElementToNgramsElement, ngramsTermText, normNgram, patchSetFromMap, replace, rootsOf, singletonNgramsTablePatch, syncResetButtons, toVersioned)
import Gargantext.Components.NgramsTable.Loader (useLoaderWithCacheAPI)
import Gargantext.Components.Nodes.Lists.Types as NT
import Gargantext.Routes (SessionRoute(..)) as R
import Gargantext.Sessions (Session, get)
import Gargantext.Types (CTabNgramType, OrderBy(..), SearchQuery, TabType, TermList(..), TermSize, termLists, termSizes)
import Gargantext.Utils (queryMatchesLabel, toggleSet, sortWith)
import Gargantext.Utils.CacheAPI as GUC
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Reload as GUR
import Gargantext.Utils.Seq as Seq
import Gargantext.Utils.Toestand as T2

here :: R2.Here
here = R2.here "Gargantext.Components.NgramsTable"

_ngramsChildren :: forall row. Lens' { ngramsChildren :: Map NgramsTerm Boolean | row } (Map NgramsTerm Boolean)
_ngramsChildren = prop (SProxy :: SProxy "ngramsChildren")

_ngramsSelection :: forall row. Lens' { ngramsSelection :: Set NgramsTerm | row } (Set NgramsTerm)
_ngramsSelection = prop (SProxy :: SProxy "ngramsSelection")

type State =
  CoreState (
    ngramsChildren   :: Map NgramsTerm Boolean
                     -- ^ Used only when grouping.
                     --   This updates the children of `ngramsParent`,
                     --   ngrams set to `true` are to be added, and `false` to
                     --   be removed.
  , ngramsParent     :: Maybe NgramsTerm -- Nothing means we are not currently grouping terms
  , ngramsSelection  :: Set NgramsTerm
                     -- ^ The set of selected checkboxes of the first column.
  )

initialState :: VersionedNgramsTable -> State
initialState (Versioned {version}) = {
    ngramsChildren:   mempty
  , ngramsLocalPatch: mempty
  , ngramsParent:     Nothing
  , ngramsSelection:  mempty
  , ngramsStagePatch: mempty
  , ngramsValidPatch: mempty
  , ngramsVersion:    version
  }

setTermListSetA :: NgramsTable -> Set NgramsTerm -> TermList -> Action
setTermListSetA ngramsTable ns new_list =
  CoreAction $ CommitPatch $ fromNgramsPatches $ PatchMap $ mapWithIndex f $ toMap ns
  where
    f :: NgramsTerm -> Unit -> NgramsPatch
    f n unit = NgramsPatch { patch_list, patch_children: mempty }
      where
        cur_list = ngramsTable ^? at n <<< _Just <<< _NgramsRepoElement <<< _list
        patch_list = maybe mempty (\c -> replace c new_list) cur_list
    toMap :: forall a. Set a -> Map a Unit
    toMap = unsafeCoerce
    -- TODO https://github.com/purescript/purescript-ordered-collections/pull/21
    --      https://github.com/purescript/purescript-ordered-collections/pull/31
    -- toMap = Map.fromFoldable

type PreConversionRows = Seq.Seq NgramsElement

type TableContainerProps =
  ( dispatch         :: Dispatch
  , ngramsChildren   :: Map NgramsTerm Boolean
  , ngramsParent     :: Maybe NgramsTerm
  , ngramsSelection  :: Set NgramsTerm
  , ngramsTable      :: NgramsTable
  , path             :: R.State PageParams
  , tabNgramType     :: CTabNgramType
  , syncResetButton  :: Array R.Element
  )

tableContainer :: Record TableContainerProps -> Record T.TableContainerProps -> R.Element
tableContainer p q = R.createElement (tableContainerCpt p) q []

tableContainerCpt :: Record TableContainerProps -> R.Component T.TableContainerProps
tableContainerCpt { dispatch
                  , ngramsChildren
                  , ngramsParent
                  , ngramsSelection
                  , ngramsTable: ngramsTableCache
                  , path: {searchQuery, termListFilter, termSizeFilter} /\ setPath
                  , tabNgramType
                  , syncResetButton
                  } = here.component "tableContainer" cpt
  where
    cpt props _ = do
      pure $ H.div {className: "container-fluid"} [
        R2.row
        [ H.div {className: "card col-12"}
          [ H.div {className: "card-header"}
            [
              R2.row [ H.div {className: "col-md-2", style: {marginTop: "6px"}}
                       [ H.div {} syncResetButton
                       , if A.null props.tableBody && searchQuery /= "" then
                           H.li { className: "list-group-item" } [
                             H.button { className: "btn btn-primary"
                                      , on: { click: const $ dispatch
                                              $ CoreAction
                                              $ addNewNgramA
                                              (normNgram tabNgramType searchQuery)
                                              CandidateTerm
                                            }
                                      }
                             [ H.text ("Add " <> searchQuery) ]
                             ] else H.div {} []
                       ]
                     , H.div {className: "col-md-2", style: {marginTop : "6px"}}
                       [ H.li {className: "list-group-item"}
                         [ R2.select { id: "picklistmenu"
                                     , className: "form-control custom-select"
                                     , defaultValue: (maybe "" show termListFilter)
                                     , on: {change: setTermListFilter <<< read <<< R.unsafeEventValue}}
                           (map optps1 termLists)]
                       ]
                     , H.div {className: "col-md-2", style: {marginTop : "6px"}}
                       [ H.li {className: "list-group-item"}
                         [ R2.select {id: "picktermtype"
                                     , className: "form-control custom-select"
                                     , defaultValue: (maybe "" show termSizeFilter)
                                     , on: {change: setTermSizeFilter <<< read <<< R.unsafeEventValue}}
                           (map optps1 termSizes)]
                       ]
                     , H.div { className: "col-md-2", style: { marginTop: "6px" } }
                       [ H.li {className: "list-group-item"}
                         [ H.div { className: "form-inline" }
                           [ H.div { className: "form-group" }
                             [ props.pageSizeControl
                             , H.label {} [ H.text " items" ]
                               --   H.div { className: "col-md-6" } [ props.pageSizeControl ]
                               -- , H.div { className: "col-md-6" } [
                               --    ]
                             ]
                           ]
                         ]
                       ]
                     , H.div {className: "col-md-4", style: {marginTop : "6px", marginBottom : "1px"}}
                       [ H.li {className: "list-group-item"}
                         [ props.pageSizeDescription
                         , props.paginationLinks
                         ]
                       ]
                     ]
            ]
          , editor
          , if (selectionsExist ngramsSelection)
            then H.li {className: "list-group-item"}
                 [selectButtons true]
            else H.div {} []
          , H.div {id: "terms_table", className: "card-body"}
            [ H.table {className: "table able"}
              [ H.thead {className: ""} [props.tableHead]
              , H.tbody {} props.tableBody
              ]
            , H.li {className: "list-group-item"}
              [ H.div { className: "row" }
                [ H.div { className: "col-md-4" }
                  [selectButtons (selectionsExist ngramsSelection)]
                , H.div {className: "col-md-4 col-md-offset-4"}
                  [props.paginationLinks]
                ]
              ]
            ]
          ]
        ]
      ]
    -- WHY setPath     f = origSetPageParams (const $ f path)
    setTermListFilter x = setPath $ _ { termListFilter = x }
    setTermSizeFilter x = setPath $ _ { termSizeFilter = x }
    setSelection = dispatch <<< setTermListSetA ngramsTableCache ngramsSelection

    editor = H.div {} $ maybe [] f ngramsParent
      where
        f ngrams = [ H.p {} [H.text $ "Editing " <> ngramsTermText ngrams]
                   , NTC.renderNgramsTree { ngramsTable
                                          , ngrams
                                          , ngramsStyle: []
                                          , ngramsClick
                                          , ngramsEdit
                                          }
                   , H.button { className: "btn btn-primary"
                              , on: {click: (const $ dispatch AddTermChildren)}
                              } [H.text "Save"]
                   , H.button { className: "btn btn-primary"
                              , on: {click: (const $ dispatch $ SetParentResetChildren Nothing)}
                              } [H.text "Cancel"]
                   ]
          where
            ngramsTable = ngramsTableCache # at ngrams
                          <<< _Just
                          <<< _NgramsRepoElement
                          <<< _children
                          %~ applyPatchSet (patchSetFromMap ngramsChildren)
            ngramsClick {depth: 1, ngrams: child} = Just $ dispatch $ ToggleChild false child
            ngramsClick _ = Nothing
            ngramsEdit  _ = Nothing

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

type CommonProps = (
    afterSync         :: Unit -> Aff Unit
  , reloadForest      :: T.Cursor T2.Reload
  , reloadRoot        :: T.Cursor T2.Reload
  , sidePanelTriggers :: Record NT.SidePanelTriggers
  , tabNgramType      :: CTabNgramType
  , tasks             :: T.Cursor (Maybe GAT.Reductor)
  , withAutoUpdate    :: Boolean
  )

type Props =
  ( cacheState :: NT.CacheState
  , mTotalRows :: Maybe Int
  , path       :: R.State PageParams
  , state      :: R.State State
  , versioned  :: VersionedNgramsTable
  | CommonProps
  )

loadedNgramsTable :: R2.Component Props
loadedNgramsTable = R.createElement loadedNgramsTableCpt

loadedNgramsTableCpt :: R.Component Props
loadedNgramsTableCpt = here.component "loadedNgramsTable" cpt where
  cpt props@{ afterSync
            , cacheState
            , mTotalRows
            , path: path@(path'@{ listIds, nodeId, params, searchQuery, scoreType, termListFilter, termSizeFilter } /\ setPath)
            , reloadForest
            , reloadRoot
            , sidePanelTriggers
            , state: (state@{ ngramsChildren
                            , ngramsLocalPatch
                            , ngramsParent
                            , ngramsSelection
                            , ngramsVersion } /\ setState)
            , tabNgramType
            , tasks
            , versioned: Versioned { data: initTable }
            , withAutoUpdate } _ = do
    pure $ R.fragment $
      autoUpdate <>
      [ H.h4 {style: {textAlign : "center"}}
        [ H.span {className: "fa fa-hand-o-down"} []
        , H.text "Extracted Terms" ]
      , search
      , T.table
        { colNames
        , container: tableContainer
          { dispatch: performAction, ngramsChildren, ngramsParent, ngramsSelection
          , ngramsTable, path, syncResetButton: [ syncResetButton ], tabNgramType }
        , params: paramsS -- TODO-LENS
        , rows: filteredConvertedRows
        , syncResetButton: [ syncResetButton ]
        , totalRecords
        , wrapColElts:
          wrapColElts { allNgramsSelected, dispatch: performAction, ngramsSelection }
        }
      , syncResetButton
      ]

      where
        afterSync' _ = do
          chartsAfterSync path' tasks nodeId reloadForest unit
          afterSync unit

        performAction = mkDispatch { filteredRows, path: path', state: state /\ setState }

        syncResetButton = syncResetButtons { afterSync: afterSync'
                                           , ngramsLocalPatch
                                           , performAction: performAction <<< CoreAction }

        autoUpdate :: Array R.Element
        autoUpdate = if withAutoUpdate then
                       [ R2.buff
                       $ autoUpdateElt
                         { duration: 5000
                         , effect: performAction
                         $ CoreAction
                         $ Synchronize { afterSync: afterSync' }
                         }
                       ]
                     else []

        totalRecords = fromMaybe (Seq.length rows) mTotalRows
        filteredConvertedRows :: T.Rows
        filteredConvertedRows = convertRow <$> filteredRows
        filteredRows :: PreConversionRows
        -- no need to filter offset if cache is off
        filteredRows = if cacheState == NT.CacheOn then T.filterRows { params } rows else rows
        ng_scores :: Map NgramsTerm (Additive Int)
        ng_scores = ngramsTable ^. _NgramsTable <<< _ngrams_scores
        rows :: PreConversionRows
        rows = orderWith (
                 Seq.mapMaybe (\(Tuple ng nre) ->
                                let Additive s = ng_scores ^. at ng <<< _Just in
                                addOcc <$> rowsFilter (ngramsRepoElementToNgramsElement ng s nre)) $
                   Map.toUnfoldable (ngramsTable ^. _NgramsTable <<< _ngrams_repo_elements)
               )
        rowsFilter :: NgramsElement -> Maybe NgramsElement
        rowsFilter ne =
           if displayRow state searchQuery ngramsTable ngramsParentRoot termListFilter termSizeFilter ne then
             Just ne
           else
             Nothing
        addOcc ngramsElement =
          let Additive occurrences = sumOccurrences ngramsTable (ngramsElementToNgramsOcc ngramsElement) in
          ngramsElement # _NgramsElement <<< _occurrences .~ occurrences

        allNgramsSelected = allNgramsSelectedOnFirstPage ngramsSelection filteredRows

        ngramsTable = applyNgramsPatches state initTable
        roots = rootsOf ngramsTable
        ngramsParentRoot :: Maybe NgramsTerm
        ngramsParentRoot =
          (\np -> ngramsTable ^? at np
                            <<< _Just
                            <<< _NgramsRepoElement
                            <<< _root
                            <<< _Just
            ) =<< ngramsParent

        convertRow ngramsElement =
          { row: NTC.renderNgramsItem { dispatch: performAction
                                      , ngrams: ngramsElement ^. _NgramsElement <<< _ngrams
                                      , ngramsElement
                                      , ngramsLocalPatch
                                      , ngramsParent
                                      , ngramsSelection
                                      , ngramsTable
                                      , sidePanelTriggers } []
          , delete: false
          }
        orderWith =
          case convOrderBy <$> params.orderBy of
            Just ScoreAsc  -> sortWith \x -> x        ^. _NgramsElement <<< _occurrences
            Just ScoreDesc -> sortWith \x -> Down $ x ^. _NgramsElement <<< _occurrences
            Just TermAsc   -> sortWith \x -> x        ^. _NgramsElement <<< _ngrams
            Just TermDesc  -> sortWith \x -> Down $ x ^. _NgramsElement <<< _ngrams
            _              -> identity -- the server ordering is enough here

        colNames = T.ColumnName <$> ["Show", "Select", "Map", "Stop", "Terms", "Score"] -- see convOrderBy
        -- This is used to *decorate* the Select header with the checkbox.
        wrapColElts scProps (T.ColumnName "Select") = const [NTC.selectionCheckbox scProps]
        wrapColElts _       (T.ColumnName "Score")  = (_ <> [H.text ("(" <> show scoreType <> ")")])
        wrapColElts _       _                       = identity
        setParams f = setPath $ \p@{params: ps} -> p {params = f ps}
        paramsS = params /\ setParams

        search :: R.Element
        search = NTC.searchInput { key: "search-input"
                                 , onSearch: setSearchQuery
                                 , searchQuery: searchQuery }
        setSearchQuery :: String -> Effect Unit
        setSearchQuery x    = do
          setPath $ _ { searchQuery    = x }
          T.changePage 1 paramsS

type MkDispatchProps = (
    filteredRows :: PreConversionRows
  , path         :: PageParams
  , state :: R.State State
  )

mkDispatch :: Record MkDispatchProps -> (Action -> Effect Unit)
mkDispatch { filteredRows
           , path
           , state: (state@{ ngramsChildren
                           , ngramsLocalPatch
                           , ngramsParent
                           , ngramsSelection
                           , ngramsVersion } /\ setState) } = performAction
  where
    allNgramsSelected = allNgramsSelectedOnFirstPage ngramsSelection filteredRows

    setParentResetChildren :: Maybe NgramsTerm -> State -> State
    setParentResetChildren p = _ { ngramsParent = p, ngramsChildren = mempty }

    performAction :: Action -> Effect Unit
    performAction (SetParentResetChildren p) =
      setState $ setParentResetChildren p
    performAction (ToggleChild b c) =
      setState $ \s@{ ngramsChildren: nc } -> s { ngramsChildren = newNC nc }
      where
        newNC nc = Map.alter (maybe (Just b) (const Nothing)) c nc
    performAction (ToggleSelect c) =
      setState $ \s@{ ngramsSelection: ns } -> s { ngramsSelection = toggleSet c ns }
    performAction ToggleSelectAll =
      setState toggler
      where
        toggler s =
          if allNgramsSelected then
            s { ngramsSelection = Set.empty :: Set NgramsTerm }
          else
            s { ngramsSelection = selectNgramsOnFirstPage filteredRows }
    performAction AddTermChildren =
      case ngramsParent of
        Nothing ->
          -- impossible but harmless
          pure unit
        Just parent -> do
          let pc = patchSetFromMap ngramsChildren
              pe = NgramsPatch { patch_list: mempty, patch_children: pc }
              pt = singletonNgramsTablePatch parent pe
          setState $ setParentResetChildren Nothing
          commitPatch (Versioned {version: ngramsVersion, data: pt}) (state /\ setState)
    performAction (CoreAction a) = coreDispatch path (state /\ setState) a


displayRow :: State -> SearchQuery -> NgramsTable -> Maybe NgramsTerm -> Maybe TermList -> Maybe TermSize -> NgramsElement -> Boolean
displayRow state@{ ngramsChildren
                 , ngramsLocalPatch
                 , ngramsParent }
           searchQuery
           ngramsTable
           ngramsParentRoot
           termListFilter
           termSizeFilter
           (NgramsElement {ngrams, root, list}) =
  (
      isNothing root
    -- ^ Display only nodes without parents
    && maybe true (_ == list) termListFilter
    -- ^ and which matches the ListType filter.
    && ngramsChildren ^. at ngrams /= Just true
    -- ^ and which are not scheduled to be added already
    && Just ngrams /= ngramsParent
    -- ^ and which are not our new parent
    && Just ngrams /= ngramsParentRoot
    -- ^ and which are not the root of our new parent
    && filterTermSize termSizeFilter ngrams
    -- ^ and which satisfies the chosen term size
    || ngramsChildren ^. at ngrams == Just false
    -- ^ unless they are scheduled to be removed.
    || NTC.tablePatchHasNgrams ngramsLocalPatch ngrams
    -- ^ unless they are being processed at the moment.
  )
    && queryMatchesLabel searchQuery (ngramsTermText ngrams)
    -- ^ and which matches the search query.


allNgramsSelectedOnFirstPage :: Set NgramsTerm -> PreConversionRows -> Boolean
allNgramsSelectedOnFirstPage selected rows = selected == (selectNgramsOnFirstPage rows)

selectNgramsOnFirstPage :: PreConversionRows -> Set NgramsTerm
selectNgramsOnFirstPage rows = Set.fromFoldable $ (view $ _NgramsElement <<< _ngrams) <$> rows


type MainNgramsTableProps = (
    cacheState        :: R.State NT.CacheState
  , defaultListId     :: Int
  , nodeId            :: Int
    -- ^ This node can be a corpus or contact.
  , path             :: R.State PageParams
  , session           :: Session
  , tabType           :: TabType
  | CommonProps
  )

mainNgramsTable :: R2.Component MainNgramsTableProps
mainNgramsTable = R.createElement mainNgramsTableCpt

mainNgramsTableCpt :: R.Component MainNgramsTableProps
mainNgramsTableCpt = here.component "mainNgramsTable" cpt
  where
    cpt props@{ afterSync
              , reloadRoot
              , tasks
              , cacheState
              , defaultListId
              , nodeId
              , path
              , session
              , sidePanelTriggers
              , tabNgramType
              , tabType
              , reloadForest
              , withAutoUpdate } _ = do

      -- let path = initialPageParams session nodeId [defaultListId] tabType

      case cacheState of
        (NT.CacheOn /\ _) -> do
          let render versioned = mainNgramsTablePaint { afterSync
                                                      , cacheState: fst cacheState
                                                      , path: fst path
                                                      , reloadForest
                                                      , reloadRoot
                                                      , sidePanelTriggers
                                                      , tabNgramType
                                                      , tasks
                                                      , versioned
                                                      , withAutoUpdate } []
          useLoaderWithCacheAPI {
              cacheEndpoint: versionEndpoint props
            , handleResponse
            , mkRequest
            , path: fst path
            , renderer: render
            }
        (NT.CacheOff /\ _) -> do
          -- path <- R.useState' path
          let render versionedWithCount = mainNgramsTablePaintNoCache { afterSync
                                                                      , cacheState: fst cacheState
                                                                      , path
                                                                      , reloadForest
                                                                      , reloadRoot
                                                                      , sidePanelTriggers
                                                                      , tabNgramType
                                                                      , tasks
                                                                      , versionedWithCount
                                                                      , withAutoUpdate } []
          useLoader (fst path) loader render

    -- NOTE With cache on
    versionEndpoint :: Record MainNgramsTableProps -> PageParams -> Aff Version
    versionEndpoint { defaultListId, nodeId, session, tabType } _ = get session $ R.GetNgramsTableVersion { listId: defaultListId, tabType } (Just nodeId)

    -- NOTE With cache off
    loader :: PageParams -> Aff VersionedWithCountNgramsTable
    loader path@{ listIds
                , nodeId
                , params: { limit, offset, orderBy }
                , searchQuery
                , session
                , tabType
                , termListFilter
                , termSizeFilter
                } =
      get session $ R.GetNgrams params (Just nodeId)
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

    -- NOTE With cache on
    mkRequest :: PageParams -> GUC.Request
    mkRequest path@{ session } = GUC.makeGetRequest session $ url path
      where
        url { listIds
            , nodeId
            , params: { limit, offset, orderBy }
            , searchQuery
            , scoreType
            , tabType
            , termListFilter
            , termSizeFilter
            } = R.GetNgramsTableAll { listIds
                                    , tabType } (Just nodeId)

    handleResponse :: VersionedNgramsTable -> VersionedNgramsTable
    handleResponse v = v

type MainNgramsTablePaintProps = (
    cacheState         :: NT.CacheState
  , path              :: PageParams
  , versioned         :: VersionedNgramsTable
  | CommonProps
  )

mainNgramsTablePaint :: R2.Component MainNgramsTablePaintProps
mainNgramsTablePaint = R.createElement mainNgramsTablePaintCpt

mainNgramsTablePaintCpt :: R.Component MainNgramsTablePaintProps
mainNgramsTablePaintCpt = here.component "mainNgramsTablePaint" cpt
  where
    cpt props@{ afterSync
              , cacheState
              , path
              , reloadForest
              , reloadRoot
              , sidePanelTriggers
              , tabNgramType
              , tasks
              , versioned
              , withAutoUpdate } _ = do
      path' <- R.useState' path
      state <- R.useState' $ initialState versioned
      pure $ loadedNgramsTable { afterSync
                               , cacheState
                               , mTotalRows: Nothing
                               , path: path'
                               , reloadForest
                               , reloadRoot
                               , sidePanelTriggers
                               , state
                               , tabNgramType
                               , tasks
                               , versioned
                               , withAutoUpdate
                               } []

type MainNgramsTablePaintNoCacheProps = (
    cacheState         :: NT.CacheState
  , path              :: R.State PageParams
  , versionedWithCount :: VersionedWithCountNgramsTable
  | CommonProps
  )

mainNgramsTablePaintNoCache :: R2.Component MainNgramsTablePaintNoCacheProps
mainNgramsTablePaintNoCache = R.createElement mainNgramsTablePaintNoCacheCpt

mainNgramsTablePaintNoCacheCpt :: R.Component MainNgramsTablePaintNoCacheProps
mainNgramsTablePaintNoCacheCpt = here.component "mainNgramsTablePaintNoCache" cpt
  where
    cpt props@{ afterSync
              , cacheState
              , path
              , reloadForest
              , reloadRoot
              , sidePanelTriggers
              , tabNgramType
              , tasks
              , versionedWithCount
              , withAutoUpdate } _ = do
      let count /\ versioned = toVersioned versionedWithCount

      state <- R.useState' $ initialState versioned

      pure $ loadedNgramsTable {
        afterSync
      , cacheState
      , mTotalRows: Just count
      , path: path
      , reloadForest
      , reloadRoot
      , sidePanelTriggers
      , state
      , tabNgramType
      , tasks
      , versioned
      , withAutoUpdate
      } []

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



