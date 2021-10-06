module Gargantext.Components.NgramsTable
  ( MainNgramsTableProps
  , CommonProps
  , mainNgramsTable
  ) where

import Gargantext.Prelude

import Data.Array as A
import Data.Either (Either)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens (to, view, (%~), (.~), (^.), (^?))
import Data.Lens.At (at)
import Data.Lens.Common (_Just)
import Data.Lens.Fold (folded)
import Data.Lens.Index (ix)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Ord.Down (Down(..))
import Data.Sequence (Seq, length) as Seq
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Gargantext.Components.App.Data (Boxes)
import Gargantext.Components.AutoUpdate (autoUpdateElt)
import Gargantext.Components.NgramsTable.Components as NTC
import Gargantext.Components.NgramsTable.Core (Action(..), CoreAction(..), CoreState, Dispatch, NgramsElement(..), NgramsPatch(..), NgramsTable, NgramsTerm, PageParams, PatchMap(..), Versioned(..), VersionedNgramsTable, VersionedWithCountNgramsTable, _NgramsElement, _NgramsRepoElement, _NgramsTable, _children, _list, _ngrams, _ngrams_repo_elements, _ngrams_scores, _occurrences, _root, addNewNgramA, applyNgramsPatches, applyPatchSet, chartsAfterSync, commitPatch, convOrderBy, coreDispatch, filterTermSize, fromNgramsPatches, ngramsRepoElementToNgramsElement, ngramsTermText, normNgram, patchSetFromMap, replace, singletonNgramsTablePatch, syncResetButtons, toVersioned)
import Gargantext.Components.NgramsTable.Loader (useLoaderWithCacheAPI)
import Gargantext.Components.Nodes.Lists.Types as NT
import Gargantext.Components.Table as TT
import Gargantext.Components.Table.Types as TT
import Gargantext.Config.REST (RESTError)
import Gargantext.Hooks.Loader (useLoaderBox)
import Gargantext.Routes (SessionRoute(..)) as R
import Gargantext.Sessions (Session, get)
import Gargantext.Types (CTabNgramType, OrderBy(..), SearchQuery, TabType, TermList(..), TermSize, termLists, termSizes)
import Gargantext.Utils (queryMatchesLabel, toggleSet, sortWith)
import Gargantext.Utils.CacheAPI as GUC
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Seq as Seq
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T
import Unsafe.Coerce (unsafeCoerce)

here :: R2.Here
here = R2.here "Gargantext.Components.NgramsTable"

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
    ngramsChildren:   Map.empty
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
    f n _unit = NgramsPatch { patch_list, patch_children: mempty }
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
  , path             :: T.Box PageParams
  , tabNgramType     :: CTabNgramType
  , syncResetButton  :: Array R.Element
  )

tableContainer :: Record TableContainerProps -> Record TT.TableContainerProps -> R.Element
tableContainer p q = R.createElement (tableContainerCpt p) q []
tableContainerCpt :: Record TableContainerProps -> R.Component TT.TableContainerProps
tableContainerCpt { dispatch
                  , ngramsChildren
                  , ngramsParent
                  , ngramsSelection
                  , ngramsTable: ngramsTableCache
                  , path
                  , tabNgramType
                  , syncResetButton
                  } = here.component "tableContainer" cpt
  where
    cpt props _ = do
      { searchQuery, termListFilter, termSizeFilter } <- T.useLive T.unequal path

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
                                              MapTerm
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
    setTermListFilter x = T.modify (_ { termListFilter = x }) path
    setTermSizeFilter x = T.modify (_ { termSizeFilter = x }) path
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

type CommonProps =
  ( afterSync      :: Unit -> Aff Unit
  , boxes          :: Boxes
  , tabNgramType   :: CTabNgramType
  , withAutoUpdate :: Boolean
  )

type PropsNoReload =
  ( cacheState :: NT.CacheState
  , mTotalRows :: Maybe Int
  , path       :: T.Box PageParams
  , state      :: T.Box State
  , versioned  :: VersionedNgramsTable
  | CommonProps
  )

type Props =
  ( reloadForest   :: T2.ReloadS
  , reloadRoot     :: T2.ReloadS
  | PropsNoReload )

loadedNgramsTable :: R2.Component PropsNoReload
loadedNgramsTable = R.createElement loadedNgramsTableCpt
loadedNgramsTableCpt :: R.Component PropsNoReload
loadedNgramsTableCpt = here.component "loadedNgramsTable" cpt where
  cpt props@{ path } _ = do
    searchQuery <- T.useFocused (_.searchQuery) (\a b -> b { searchQuery = a }) path

    pure $ R.fragment $
      [ loadedNgramsTableHeader { searchQuery } []
      , loadedNgramsTableBody props [] ]

type LoadedNgramsTableHeaderProps =
  ( searchQuery :: T.Box SearchQuery )

loadedNgramsTableHeader :: R2.Component LoadedNgramsTableHeaderProps
loadedNgramsTableHeader = R.createElement loadedNgramsTableHeaderCpt
loadedNgramsTableHeaderCpt :: R.Component LoadedNgramsTableHeaderProps
loadedNgramsTableHeaderCpt = here.component "loadedNgramsTableHeader" cpt where
  cpt { searchQuery } _ = do
    pure $ R.fragment
      [ H.h4 {style: {textAlign : "center"}}
        [ H.span {className: "fa fa-hand-o-down"} []
        , H.text "Extracted Terms" ]
      , NTC.searchInput { key: "search-input"
                        , searchQuery }
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
      , versioned: Versioned { data: initTable }
      , withAutoUpdate } _ = do
    state'@{ ngramsChildren, ngramsLocalPatch, ngramsParent, ngramsSelection } <- T.useLive T.unequal state
    path'@{ scoreType, termListFilter, termSizeFilter } <- T.useLive T.unequal path
    params <- T.useFocused (_.params) (\a b -> b { params = a }) path
    params'@{ orderBy } <- T.useLive T.unequal params
    searchQuery <- T.useFocused (_.searchQuery) (\a b -> b { searchQuery = a }) path
    searchQuery' <- T.useLive T.unequal searchQuery

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
        rows = ngramsTableOrderWith orderBy (
                 Seq.mapMaybe rowMap $
                   Map.toUnfoldable (ngramsTable ^. _NgramsTable <<< _ngrams_repo_elements)
               )
        rowsFilter :: NgramsElement -> Maybe NgramsElement
        rowsFilter ngramsElement =
          if displayRow { ngramsElement
                        , ngramsParentRoot
                        , searchQuery: searchQuery'
                        , state: state'
                        , termListFilter
                        , termSizeFilter } then
            Just ngramsElement
          else
            Nothing

        performAction = mkDispatch { filteredRows
                                   , path: path'
                                   , state
                                   , state' }

        -- filteredRows :: PreConversionRows
        -- no need to filter offset if cache is off
        filteredRows = if cacheState == NT.CacheOn then TT.filterRows { params: params' } rows else rows
        filteredConvertedRows :: TT.Rows
        filteredConvertedRows = convertRow <$> filteredRows

        convertRow ngramsElement =
          { row: NTC.renderNgramsItem { dispatch: performAction
                                      , ngrams: ngramsElement ^. _NgramsElement <<< _ngrams
                                      , ngramsElement
                                      , ngramsLocalPatch
                                      , ngramsParent
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

    pure $ R.fragment
      [ TT.table
        { colNames
        , container: tableContainer
          { dispatch: performAction
          , ngramsChildren
          , ngramsParent
          , ngramsSelection
          , ngramsTable
          , path
          , syncResetButton: [ syncResetButton ]
          , tabNgramType }
        , params
        , rows: filteredConvertedRows
        , syncResetButton: [ syncResetButton ]
        , totalRecords
        , wrapColElts:
          wrapColElts { allNgramsSelected, dispatch: performAction, ngramsSelection } scoreType
        }
      , syncResetButton
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
wrapColElts scProps _         (TT.ColumnName "Select") = const [NTC.selectionCheckbox scProps]
wrapColElts _       scoreType (TT.ColumnName "Score")  = (_ <> [H.text ("(" <> show scoreType <> ")")])
wrapColElts _       _         _                        = identity

type MkDispatchProps = (
    filteredRows :: PreConversionRows
  , path         :: PageParams
  , state        :: T.Box State
  , state'       :: State
  )

mkDispatch :: Record MkDispatchProps -> (Action -> Effect Unit)
mkDispatch { filteredRows
           , path
           , state
           , state': { ngramsChildren
                     , ngramsParent
                     , ngramsSelection } } = performAction
  where
    allNgramsSelected = allNgramsSelectedOnFirstPage ngramsSelection filteredRows

    setParentResetChildren :: Maybe NgramsTerm -> State -> State
    setParentResetChildren p = _ { ngramsParent = p, ngramsChildren = Map.empty }

    performAction :: Action -> Effect Unit
    performAction (SetParentResetChildren p) =
      T.modify_ (setParentResetChildren p) state
    performAction (ToggleChild b c) =
      T.modify_ (\s@{ ngramsChildren: nc } -> s { ngramsChildren = newNC nc }) state
      where
        newNC nc = Map.alter (maybe (Just b) (const Nothing)) c nc
    performAction (ToggleSelect c) =
      T.modify_ (\s@{ ngramsSelection: ns } -> s { ngramsSelection = toggleSet c ns }) state
    performAction ToggleSelectAll =
      T.modify_ toggler state
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
          T.modify_ (setParentResetChildren Nothing) state
          commitPatch pt state
    performAction (CoreAction a) = coreDispatch path state a


displayRow :: { ngramsElement    :: NgramsElement
              , ngramsParentRoot :: Maybe NgramsTerm
              , searchQuery      :: SearchQuery
              , state            :: State
              , termListFilter   :: Maybe TermList
              , termSizeFilter   :: Maybe TermSize } -> Boolean
displayRow { ngramsElement: NgramsElement {ngrams, root, list}
           , ngramsParentRoot
           , state: { ngramsChildren
                    , ngramsLocalPatch
                    , ngramsParent }
           , searchQuery
           , termListFilter
           , termSizeFilter } =
  (
    -- isNothing root
    -- ^ Display only nodes without parents
    -- ^^ (?) allow child nodes to be searched (see #340)
       maybe true (_ == list) termListFilter
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
    cacheState    :: T.Box NT.CacheState
  , defaultListId :: Int
    -- ^ This node can be a corpus or contact.
  , path          :: T.Box PageParams
  , session       :: Session
  , tabType       :: TabType
  | CommonProps
  )

mainNgramsTable :: R2.Component MainNgramsTableProps
mainNgramsTable = R.createElement mainNgramsTableCpt
mainNgramsTableCpt :: R.Component MainNgramsTableProps
mainNgramsTableCpt = here.component "mainNgramsTable" cpt
  where
    cpt props@{ cacheState } _ = do
      cacheState' <- T.useLive T.unequal cacheState

      -- let path = initialPageParams session nodeId [defaultListId] tabType

      case cacheState' of
        NT.CacheOn -> pure $ mainNgramsTableCacheOn props []
        NT.CacheOff -> pure $ mainNgramsTableCacheOff props []

mainNgramsTableCacheOn :: R2.Component MainNgramsTableProps
mainNgramsTableCacheOn = R.createElement mainNgramsTableCacheOnCpt
mainNgramsTableCacheOnCpt :: R.Component MainNgramsTableProps
mainNgramsTableCacheOnCpt = here.component "mainNgramsTableCacheOn" cpt where
  cpt { afterSync
      , boxes
      , defaultListId
      , path
      , tabNgramType
      , withAutoUpdate } _ = do

    -- let path = initialPageParams session nodeId [defaultListId] tabType

    path' <- T.useLive T.unequal path
    let render versioned = mainNgramsTablePaint { afterSync
                                                , boxes
                                                , cacheState: NT.CacheOn
                                                , path
                                                , tabNgramType
                                                , versioned
                                                , withAutoUpdate } []
    useLoaderWithCacheAPI {
        cacheEndpoint: versionEndpoint { defaultListId, path: path' }
      , errorHandler
      , handleResponse
      , mkRequest
      , path: path'
      , renderer: render
      }
  versionEndpoint { defaultListId, path: { nodeId, tabType, session } } _ = get session $ R.GetNgramsTableVersion { listId: defaultListId, tabType } (Just nodeId)
  errorHandler err = here.log2 "[mainNgramsTable] RESTError" err
  mkRequest :: PageParams -> GUC.Request
  mkRequest path@{ session } = GUC.makeGetRequest session $ url path
    where
      url { listIds
          , nodeId
          , tabType
          } = R.GetNgramsTableAll { listIds
                                  , tabType } (Just nodeId)
  handleResponse :: VersionedNgramsTable -> VersionedNgramsTable
  handleResponse v = v

mainNgramsTableCacheOff :: R2.Component MainNgramsTableProps
mainNgramsTableCacheOff = R.createElement mainNgramsTableCacheOffCpt
mainNgramsTableCacheOffCpt :: R.Component MainNgramsTableProps
mainNgramsTableCacheOffCpt = here.component "mainNgramsTableCacheOff" cpt where
  cpt { afterSync
      , boxes
      , defaultListId
      , path
      , tabNgramType
      , withAutoUpdate } _ = do
    let render versionedWithCount = mainNgramsTablePaintNoCache { afterSync
                                                                , boxes
                                                                , cacheState: NT.CacheOff
                                                                , path
                                                                , tabNgramType
                                                                , versionedWithCount
                                                                , withAutoUpdate } []
    useLoaderBox { errorHandler
                 , loader
                 , path
                 , render }

  errorHandler err = here.log2 "[mainNgramsTable] RESTError" err

  -- NOTE With cache off
  loader :: PageParams -> Aff (Either RESTError VersionedWithCountNgramsTable)
  loader { listIds
         , nodeId
         , params: { limit, offset }
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


type MainNgramsTablePaintProps = (
    cacheState :: NT.CacheState
  , path       :: T.Box PageParams
  , versioned  :: VersionedNgramsTable
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
        , tabNgramType
        , versioned
        , withAutoUpdate } _ = do
      state <- T.useBox $ initialState versioned

      pure $ loadedNgramsTable { afterSync
                               , boxes
                               , cacheState
                               , mTotalRows: Nothing
                               , path
                               , state
                               , tabNgramType
                               , versioned
                               , withAutoUpdate
                               } []

type MainNgramsTablePaintNoCacheProps = (
    cacheState         :: NT.CacheState
  , path               :: T.Box PageParams
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
        , tabNgramType
        , versionedWithCount
        , withAutoUpdate } _ = do
      -- TODO This is lame, make versionedWithCount a proper box?
      let count /\ versioned = toVersioned versionedWithCount

      state <- T.useBox $ initialState versioned

      pure $ loadedNgramsTable { afterSync
                               , boxes
                               , cacheState
                               , mTotalRows: Just count
                               , path
                               , state
                               , tabNgramType
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
