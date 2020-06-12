module Gargantext.Components.NgramsTable
  ( MainNgramsTableProps
  , mainNgramsTable
  ) where

import Data.Array as A
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens (Lens', to, (%~), (.~), (^.), (^?))
import Data.Lens.At (at)
import Data.Lens.Common (_Just)
import Data.Lens.Fold (folded)
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.List (List, filter, length) as L
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Ord.Down (Down(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Prelude (class Show, Unit, bind, const, discard, identity, map, mempty, not, pure, show, unit, (#), ($), (&&), (/=), (<$>), (<<<), (<>), (=<<), (==), (||))
import Reactix as R
import Reactix.DOM.HTML as H
import Unsafe.Coerce (unsafeCoerce)

import Gargantext.Components.AutoUpdate (autoUpdateElt)
import Gargantext.Components.Loader (loader)
import Gargantext.Components.LoadingSpinner (loadingSpinner)
import Gargantext.Components.NgramsTable.Core (Action(..), CoreState, Dispatch, NgramsElement(..), NgramsPatch(..), NgramsTable, NgramsTerm, PageParams, PatchMap(..), Versioned(..), VersionedNgramsTable, _NgramsElement, _NgramsTable, _children, _list, _ngrams, _occurrences, _root, addNewNgram, applyNgramsPatches, applyPatchSet, commitPatchR, convOrderBy, filterTermSize, fromNgramsPatches, initialPageParams, loadNgramsTableAll, ngramsTermText, normNgram, patchSetFromMap, replace, rootsOf, singletonNgramsTablePatch, syncPatchesR)
import Gargantext.Components.NgramsTable.Components as NTC
import Gargantext.Components.Table as T
import Gargantext.Sessions (Session)
import Gargantext.Types (CTabNgramType, OrderBy(..), SearchQuery, TabType, TermList(..), TermSize, readTermList, readTermSize, termLists, termSizes)
import Gargantext.Utils (queryMatchesLabel, toggleSet)
import Gargantext.Utils.List (sortWith) as L
import Gargantext.Utils.Reactix as R2

type State' =
  CoreState
  ( ngramsParent     :: Maybe NgramsTerm -- Nothing means we are not currently grouping terms
  , ngramsChildren   :: Map NgramsTerm Boolean
                     -- ^ Used only when grouping.
                     --   This updates the children of `ngramsParent`,
                     --   ngrams set to `true` are to be added, and `false` to
                     --   be removed.
  , ngramsSelection  :: Set NgramsTerm
                     -- ^ The set of selected checkboxes of the first column.
  )

_ngramsChildren :: forall row. Lens' { ngramsChildren :: Map NgramsTerm Boolean | row } (Map NgramsTerm Boolean)
_ngramsChildren = prop (SProxy :: SProxy "ngramsChildren")

_ngramsSelection :: forall row. Lens' { ngramsSelection :: Set NgramsTerm | row } (Set NgramsTerm)
_ngramsSelection = prop (SProxy :: SProxy "ngramsSelection")

initialState' :: VersionedNgramsTable -> State'
initialState' (Versioned {version}) =
  { ngramsChildren:   mempty
  , ngramsLocalPatch: mempty
  , ngramsParent:     Nothing
  , ngramsSelection:  mempty
  , ngramsStagePatch: mempty
  , ngramsValidPatch: mempty
  , ngramsVersion:    version
  }

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
  CommitPatch $ fromNgramsPatches $ PatchMap $ mapWithIndex f $ toMap ns
  where
    f :: NgramsTerm -> Unit -> NgramsPatch
    f n unit = NgramsPatch { patch_list, patch_children: mempty }
      where
        cur_list = ngramsTable ^? at n <<< _Just <<< _NgramsElement <<< _list
        patch_list = maybe mempty (\c -> replace c new_list) cur_list
    toMap :: forall a. Set a -> Map a Unit
    toMap = unsafeCoerce
    -- TODO https://github.com/purescript/purescript-ordered-collections/pull/21
    -- toMap = Map.fromFoldable

addNewNgramA :: NgramsTerm -> Action
addNewNgramA ngram = CommitPatch $ addNewNgram ngram CandidateTerm

type PreConversionRows = L.List (Tuple NgramsTerm NgramsElement)

type TableContainerProps =
  ( dispatch         :: Dispatch
  , ngramsChildren   :: Map NgramsTerm Boolean
  , ngramsParent     :: Maybe NgramsTerm
  , ngramsSelection  :: Set NgramsTerm
  , ngramsTable      :: NgramsTable
  , path             :: R.State PageParams
  , tabNgramType     :: CTabNgramType
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
                  } = R.hooksComponent "G.C.NT.tableContainer" cpt
  where
    cpt props _ = do
      pure $ H.div {className: "container-fluid"} [
        H.div {className: "jumbotron1"}
        [ R2.row
          [ H.div {className: "panel panel-default"}
            [ H.div {className: "panel-heading"} [
              R2.row
              [ H.div {className: "col-md-2", style: {marginTop: "6px"}}
                [
                  if A.null props.tableBody && searchQuery /= "" then
                    H.li { className: "list-group-item" } [
                      H.button { className: "btn btn-primary"
                                , on: { click: const $ dispatch
                                      $ addNewNgramA
                                      $ normNgram tabNgramType searchQuery
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
                              , on: {change: setTermListFilter <<< readTermList <<< R2.unsafeEventValue}}
                    (map optps1 termLists)]
                ]
              , H.div {className: "col-md-2", style: {marginTop : "6px"}}
                [ H.li {className: "list-group-item"}
                  [ R2.select {id: "picktermtype"
                              , className: "form-control custom-select"
                              , defaultValue: (maybe "" show termSizeFilter)
                              , on: {change: setTermSizeFilter <<< readTermSize <<< R2.unsafeEventValue}}
                    (map optps1 termSizes)]
                ]
              , H.div { className: "col-md-2", style: { marginTop: "6px" } } [
                  H.li {className: "list-group-item"} [
                     H.div { className: "form-inline" } [
                    H.div { className: "form-group" } [
                       props.pageSizeControl
                     , H.label {} [ H.text " items" ]
                    --   H.div { className: "col-md-6" } [ props.pageSizeControl ]
                    -- , H.div { className: "col-md-6" } [
                    --    ]
                    ]
                    ]
                  ]
                ]
              , H.div {className: "col-md-4", style: {marginTop : "6px", marginBottom : "1px"}} [
                   H.li {className: "list-group-item"} [
                        props.pageSizeDescription
                      , props.paginationLinks
                      ]
                   ]
              ]
              ]
            , editor
            , if (selectionsExist ngramsSelection) then
                H.li {className: "list-group-item"} [
                  selectButtons true
                ] else
                H.div {} []
            , H.div {id: "terms_table", className: "panel-body"}
              [ H.table {className: "table able"}
                [ H.thead {className: "tableHeader"} [props.tableHead]
                , H.tbody {} props.tableBody
                ]

              , H.li {className: "list-group-item"} [
                 H.div { className: "row" } [
                    H.div { className: "col-md-4" } [
                       selectButtons (selectionsExist ngramsSelection)
                      ]
                    , H.div { className: "col-md-4 col-md-offset-4" } [
                       props.paginationLinks
                      ]
                    ]
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
                   , H.button { className: "btn btn-secondary"
                              , on: {click: (const $ dispatch $ SetParentResetChildren Nothing)}
                              } [H.text "Cancel"]
                   ]
          where
            ngramsTable = ngramsTableCache # at ngrams
                          <<< _Just
                          <<< _NgramsElement
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
                , on: { click: const $ setSelection GraphTerm }
                } [ H.text "Map" ]
        , H.button { className: "btn btn-primary"
                  , on: { click: const $ setSelection StopTerm }
                  } [ H.text "Stop" ]
        , H.button { className: "btn btn-primary"
                  , on: { click: const $ setSelection CandidateTerm }
                  } [ H.text "Candidate" ]
      ]

-- NEXT
type Props =
  ( path         :: R.State PageParams
  , state        :: R.State State
  , tabNgramType :: CTabNgramType
  , versioned    :: VersionedNgramsTable
  , withAutoUpdate :: Boolean
  )

loadedNgramsTable :: Record Props -> R.Element
loadedNgramsTable p = R.createElement loadedNgramsTableCpt p []

loadedNgramsTableCpt :: R.Component Props
loadedNgramsTableCpt = R.hooksComponent "G.C.NT.loadedNgramsTable" cpt
  where
    cpt { path: path@(path'@{searchQuery, scoreType, params, termListFilter, termSizeFilter} /\ setPath)
        , state: (state@{ ngramsChildren
                        , ngramsLocalPatch
                        , ngramsParent
                        , ngramsSelection
                        , ngramsVersion } /\ setState)
        , tabNgramType
        , versioned: Versioned { data: initTable }
        , withAutoUpdate } _ = do

      pure $ R.fragment $
        autoUpdate <> resetSaveButtons <> [
          H.h4 {style: {textAlign : "center"}} [
              H.span {className: "glyphicon glyphicon-hand-down"} []
            , H.text "Extracted Terms"
            ]
        , search
        , T.table { colNames
                  , container: tableContainer { dispatch: performAction
                                              , ngramsChildren
                                              , ngramsParent
                                              , ngramsSelection
                                              , ngramsTable
                                              , path
                                              , tabNgramType
                                              }
                  , params: params /\ setParams -- TODO-LENS
                  , rows: filteredConvertedRows
                  , totalRecords
                  , wrapColElts: wrapColElts { allNgramsSelected
                                             , dispatch: performAction
                                             , ngramsSelection
                                             }
                  }
        ] <> resetSaveButtons
      where
        autoUpdate :: Array R.Element
        autoUpdate = if withAutoUpdate then [ R2.buff $ autoUpdateElt { duration: 5000, effect: performAction Synchronize } ] else []
        resetButton :: R.Element
        resetButton = H.button { className: "btn btn-primary"
                               , on: { click: \_ -> performAction ResetPatches } } [ H.text "Reset" ]
        saveButton :: R.Element
        saveButton = H.button { className: "btn btn-primary"
                              , on: { click: \_ -> performAction Synchronize }} [ H.text "Save" ]
        resetSaveButtons :: Array R.Element
        resetSaveButtons = if ngramsLocalPatch == mempty then [] else
          [ H.div {} [ resetButton, saveButton ] ]

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
        performAction Synchronize = syncPatchesR path' (state /\ setState)
        performAction (CommitPatch pt) =
          commitPatchR (Versioned {version: ngramsVersion, data: pt}) (state /\ setState)
        performAction ResetPatches =
          setState $ \s -> s { ngramsLocalPatch = { ngramsNewElems: mempty, ngramsPatches: mempty } }
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
              commitPatchR (Versioned {version: ngramsVersion, data: pt}) (state /\ setState)

        totalRecords = L.length rows
        filteredConvertedRows :: T.Rows
        filteredConvertedRows = convertRow <$> filteredRows
        filteredRows :: PreConversionRows
        filteredRows = T.filterRows { params } rows
        rows :: PreConversionRows
        rows = orderWith (
          addOccT <$> (
             L.filter rowsFilterT $ Map.toUnfoldable (ngramsTable ^. _NgramsTable)
             )
          )
        rowsFilter :: NgramsElement -> Boolean
        rowsFilter = displayRow state searchQuery ngramsTable ngramsParentRoot termListFilter termSizeFilter
        rowsFilterT = rowsFilter <<< snd
        addOccWithFilter ne ngramsElement =
          if rowsFilter ngramsElement then
            Just $ addOcc ne ngramsElement
          else
            Nothing
        addOcc ne ngramsElement =
          let Additive occurrences = sumOccurrences ngramsTable ngramsElement in
          ngramsElement # _NgramsElement <<< _occurrences .~ occurrences
        addOccT (Tuple ne ngramsElement) = Tuple ne $ addOcc ne ngramsElement

        allNgramsSelected = allNgramsSelectedOnFirstPage ngramsSelection filteredRows

        ngramsTable = applyNgramsPatches state initTable
        roots = rootsOf ngramsTable
        ngramsParentRoot :: Maybe NgramsTerm
        ngramsParentRoot =
          (\np -> ngramsTable ^? at np
                            <<< _Just
                            <<< _NgramsElement
                            <<< _root
                            <<< _Just
            ) =<< ngramsParent

        convertRow (Tuple ngrams ngramsElement) =
          { row: NTC.renderNgramsItem { dispatch: performAction
                                      , ngrams
                                      , ngramsElement
                                      , ngramsLocalPatch
                                      , ngramsParent
                                      , ngramsSelection
                                      , ngramsTable }
          , delete: false
          }
        orderWith =
          case convOrderBy <$> params.orderBy of
            Just ScoreAsc  -> L.sortWith \x -> (snd x)        ^. _NgramsElement <<< _occurrences
            Just ScoreDesc -> L.sortWith \x -> Down $ (snd x) ^. _NgramsElement <<< _occurrences
            Just TermAsc   -> L.sortWith \x -> (snd x)        ^. _NgramsElement <<< _ngrams
            Just TermDesc  -> L.sortWith \x -> Down $ (snd x) ^. _NgramsElement <<< _ngrams
            _              -> identity -- the server ordering is enough here

        colNames = T.ColumnName <$> ["Select", "Map", "Stop", "Terms", "Score"] -- see convOrderBy
        -- This is used to *decorate* the Select header with the checkbox.
        wrapColElts scProps (T.ColumnName "Select") = const [NTC.selectionCheckbox scProps]
        wrapColElts _       (T.ColumnName "Score")  = (_ <> [H.text ("(" <> show scoreType <> ")")])
        wrapColElts _       _                       = identity
        setParams f = setPath $ \p@{params: ps} -> p {params = f ps}

        search :: R.Element
        search = NTC.searchInput { key: "search-input"
                                 , onSearch: setSearchQuery
                                 , searchQuery: searchQuery }
        setSearchQuery :: String -> Effect Unit
        setSearchQuery x    = setPath $ _ { searchQuery    = x }


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
selectNgramsOnFirstPage rows = Set.fromFoldable $ fst <$> rows


type MainNgramsTableProps =
  ( nodeId        :: Int
    -- ^ This node can be a corpus or contact.
  , defaultListId :: Int
  , tabType       :: TabType
  , session       :: Session
  , tabNgramType  :: CTabNgramType
  , withAutoUpdate :: Boolean
  )

mainNgramsTable :: Record MainNgramsTableProps -> R.Element
mainNgramsTable props = R.createElement mainNgramsTableCpt props []

mainNgramsTableCpt :: R.Component MainNgramsTableProps
mainNgramsTableCpt = R.hooksComponent "G.C.NT.mainNgramsTable" cpt
  where
    cpt {nodeId, defaultListId, tabType, session, tabNgramType, withAutoUpdate} _ = do
      let path = initialPageParams session nodeId [defaultListId] tabType

      pure $ loader path loadNgramsTableAll \loaded -> do
        case Map.lookup tabType loaded of
          Just (versioned :: VersionedNgramsTable) ->
            mainNgramsTablePaint {path, tabNgramType, versioned, withAutoUpdate}
          Nothing -> loadingSpinner {}

type MainNgramsTablePaintProps =
  (
    path :: PageParams
  , tabNgramType  :: CTabNgramType
  , versioned :: VersionedNgramsTable
  , withAutoUpdate :: Boolean
  )

mainNgramsTablePaint :: Record MainNgramsTablePaintProps -> R.Element
mainNgramsTablePaint p = R.createElement mainNgramsTablePaintCpt p []

mainNgramsTablePaintCpt :: R.Component MainNgramsTablePaintProps
mainNgramsTablePaintCpt = R.hooksComponent "G.C.NT.mainNgramsTablePaint" cpt
  where
    cpt {path, tabNgramType, versioned, withAutoUpdate} _ = do
      pathS <- R.useState' path
      state <- R.useState' $ initialState versioned
      pure $ loadedNgramsTable {
        path: pathS
      , state
      , tabNgramType
      , versioned
      , withAutoUpdate
      }

sumOccurrences :: NgramsTable -> NgramsElement -> Additive Int
sumOccurrences ngramsTable (NgramsElement {occurrences, children}) =
    Additive occurrences <> children ^. folded <<< to (sumOccurrences' ngramsTable)
    where
      sumOccurrences' :: NgramsTable -> NgramsTerm -> Additive Int
      sumOccurrences' nt label =
          nt ^. ix label <<< to (sumOccurrences nt)

optps1 :: forall a. Show a => { desc :: String, mval :: Maybe a } -> R.Element
optps1 { desc, mval } = H.option { value: value } [H.text desc]
  where value = maybe "" show mval
