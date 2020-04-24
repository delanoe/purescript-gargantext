module Gargantext.Components.NgramsTable
  ( MainNgramsTableProps
  , mainNgramsTable
  ) where

import Data.Array as A
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens (Lens', to, view, (%~), (.~), (^.), (^..), (^?))
import Data.Lens.At (at)
import Data.Lens.Common (_Just)
import Data.Lens.Fold (folded)
import Data.Lens.Index (ix)
import Data.Lens.Record (prop)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Monoid.Additive (Additive(..))
import Data.Ord.Down (Down(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Gargantext.Components.AutoUpdate (autoUpdateElt)
import Gargantext.Components.Loader (loader)
import Gargantext.Components.LoadingSpinner (loadingSpinner)
import Gargantext.Components.NgramsTable.Core (CoreState, NgramsElement(..), NgramsPatch(..), NgramsTable, NgramsTablePatch, NgramsTerm, PageParams, PatchMap(..), Replace, Versioned(..), VersionedNgramsTable, _NgramsElement, _NgramsTable, _PatchMap, _children, _list, _ngrams, _occurrences, _root, addNewNgram, applyNgramsPatches, applyPatchSet, commitPatch, convOrderBy, fromNgramsPatches, initialPageParams, loadNgramsTableAll, ngramsTermText, normNgram, patchSetFromMap, replace, rootsOf, singletonNgramsTablePatch, syncPatches)
import Gargantext.Components.Table as T
import Gargantext.Sessions (Session)
import Gargantext.Types (CTabNgramType, OrderBy(..), TabType, TermList(..), readTermList, readTermSize, termLists, termSizes)
import Gargantext.Utils (queryMatchesLabel)
import Gargantext.Utils.Reactix as R2
import Prelude (class Show, Unit, bind, const, discard, identity, map, mempty, not, otherwise, pure, show, unit, (#), ($), (&&), (+), (/=), (<$>), (<<<), (<>), (=<<), (==), (||))
import React (ReactClass, Children)
import React.DOM (a, span, text)
import React.DOM.Props (onClick, style)
import React.DOM.Props as DOM
import Reactix as R
import Reactix.DOM.HTML as H
import Thermite (modifyState_)
import Thermite as Thermite
import Unsafe.Coerce (unsafeCoerce)

type State =
  CoreState
  ( ngramsParent     :: Maybe NgramsTerm -- Nothing means we are not currently grouping terms
  , ngramsChildren   :: Map NgramsTerm Boolean
                     -- ^ Used only when grouping.
                     --   This updates the children of `ngramsParent`,
                     --   ngrams set to `true` are to be added, and `false` to
                     --   be removed.
  , ngramsSelection  :: Set NgramsTerm
                     -- ^ The set of selected checkboxes of the first column.
  , ngramsSelectAll  :: Boolean
                     -- ^ The checkbox to select all the checkboxes of the first column.
  )

_ngramsChildren :: forall row. Lens' { ngramsChildren :: Map NgramsTerm Boolean | row } (Map NgramsTerm Boolean)
_ngramsChildren = prop (SProxy :: SProxy "ngramsChildren")

_ngramsSelectAll :: forall row. Lens' { ngramsSelectAll :: Boolean | row } Boolean
_ngramsSelectAll = prop (SProxy :: SProxy "ngramsSelectAll")

_ngramsSelection :: forall row. Lens' { ngramsSelection :: Set NgramsTerm | row } (Set NgramsTerm)
_ngramsSelection = prop (SProxy :: SProxy "ngramsSelection")

initialState :: VersionedNgramsTable -> State
initialState (Versioned {version}) =
  { ngramsLocalPatch: mempty
  , ngramsStagePatch: mempty
  , ngramsValidPatch: mempty
  , ngramsVersion:    version
  , ngramsParent:     Nothing
  , ngramsChildren:   mempty
  , ngramsSelectAll:  false
  , ngramsSelection:  mempty
  }

data Action
  = CommitPatch NgramsTablePatch
  | SetParentResetChildren (Maybe NgramsTerm)
  -- ^ This sets `ngramsParent` and resets `ngramsChildren`.
  | ToggleChild Boolean NgramsTerm
  -- ^ Toggles the NgramsTerm in the `PatchSet` `ngramsChildren`.
  -- If the `Boolean` is `true` it means we want to add it if it is not here,
  -- if it is `false` it is meant to be removed if not here.
  | AddTermChildren
  | Synchronize
  | ToggleSelect NgramsTerm
  -- ^ Toggles the NgramsTerm in the `Set` `ngramsSelection`.
  | ToggleSelectAll
  | ResetPatches

setTermListA :: NgramsTerm -> Replace TermList -> Action
setTermListA n patch_list =
  CommitPatch $
    singletonNgramsTablePatch n $
    NgramsPatch { patch_list, patch_children: mempty }

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

type Dispatch = Action -> Effect Unit

type TableContainerProps =
  ( dispatch        :: Dispatch
  , ngramsChildren  :: Map NgramsTerm Boolean
  , ngramsParent    :: Maybe NgramsTerm
  , ngramsSelectAll :: Boolean
  , ngramsSelection :: Set NgramsTerm
  , ngramsTable     :: NgramsTable
  , path            :: R.State PageParams
  , tabNgramType    :: CTabNgramType
  )

tableContainer :: Record TableContainerProps -> Record T.TableContainerProps -> R.Element
tableContainer p q = R.createElement (tableContainerCpt p) q []

tableContainerCpt :: Record TableContainerProps -> R.Component T.TableContainerProps
tableContainerCpt { dispatch
                  , ngramsChildren
                  , ngramsParent
                  , ngramsSelectAll
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
            [ H.div {className: "panel-heading"}
              [ H.h2 {className: "panel-title", style: {textAlign : "center"}}
                [ H.span {className: "glyphicon glyphicon-hand-down"} []
                , H.text "Extracted Terms"
                ]
              , R2.row
                [ H.div {className: "col-md-3", style: {marginTop: "6px"}}
                  [ H.input { className: "form-control"
                            , name: "search"
                            , placeholder: "Search"
                            , type: "value"
                            , defaultValue: searchQuery
                            , on: {input: setSearchQuery <<< R2.unsafeEventValue}}
                  , H.div {} (
                      if A.null props.tableBody && searchQuery /= "" then [
                        H.button { className: "btn btn-primary"
                                , on: {click: const $ dispatch
                                        $ addNewNgramA
                                        $ normNgram tabNgramType searchQuery
                                      }
                                }
                        [ H.text ("Add " <> searchQuery) ]
                        ] else []
                      )
                  ]
                , H.div {className: "col-md-2", style: {marginTop : "6px"}}
                  [ H.li {className: " list-group-item"}
                    [ R2.select { id: "picklistmenu"
                                , className: "form-control custom-select"
                                , defaultValue: (maybe "" show termListFilter)
                                , on: {change: setTermListFilter <<< readTermList <<< R2.unsafeEventValue}}
                      (map optps1 termLists)]]
                , H.div {className: "col-md-2", style: {marginTop : "6px"}}
                  [ H.li {className: "list-group-item"}
                    [ R2.select {id: "picktermtype"
                                , className: "form-control custom-select"
                                , defaultValue: (maybe "" show termSizeFilter)
                                , on: {change: setTermSizeFilter <<< readTermSize <<< R2.unsafeEventValue}}
                      (map optps1 termSizes)]]
                , H.div {className: "col-md-4", style: {marginTop : "6px", marginBottom : "1px"}}
                  [ H.li {className: " list-group-item"}
                    [ props.pageSizeDescription
                    , props.pageSizeControl
                    , H.text " items / "
                    , props.paginationLinks]]
                ]
              ]
            , H.div {}
              (maybe [] (\ngrams ->
                          let
                            ngramsTable =
                              ngramsTableCache # at ngrams
                              <<< _Just
                              <<< _NgramsElement
                              <<< _children
                              %~ applyPatchSet (patchSetFromMap ngramsChildren)
                            ngramsClick {depth: 1, ngrams: child} =
                              Just $ dispatch $ ToggleChild false child
                            ngramsClick _ = Nothing
                            ngramsEdit  _ = Nothing
                          in
                           [ H.p {} [H.text $ "Editing " <> ngramsTermText ngrams]
                           , renderNgramsTree { ngramsTable, ngrams, ngramsStyle: [], ngramsClick, ngramsEdit }
                           , H.button {className: "btn btn-primary", on: {click: (const $ dispatch AddTermChildren)}} [H.text "Save"]
                           , H.button {className: "btn btn-secondary", on: {click: (const $ dispatch $ SetParentResetChildren Nothing)}} [H.text "Cancel"]
                           ]) ngramsParent)
            , selectAllButtons ngramsSelectAll
            , H.div {id: "terms_table", className: "panel-body"}
              [ H.table {className: "table able"}
                [ H.thead {className: "tableHeader"} [props.tableHead]
                , H.tbody {} props.tableBody] ]

            , selectAllButtons ngramsSelectAll
            ]
          ]
        ]
      ]
    -- WHY setPath     f = origSetPageParams (const $ f path)
    setSearchQuery    x = setPath $ _ { searchQuery = x }
    setTermListFilter x = setPath $ _ { termListFilter = x }
    setTermSizeFilter x = setPath $ _ { termSizeFilter = x }
    setSelection = dispatch <<< setTermListSetA ngramsTableCache ngramsSelection

    selectAllButtons false = H.div {} []
    selectAllButtons true =
      H.li {className: " list-group-item"} [
        H.button { className: "btn btn-primary"
                , on: {click: const $ setSelection GraphTerm }
                } [ H.text "Map" ]
        , H.button { className: "btn btn-primary"
                  , on: {click: const $ setSelection StopTerm }
                  } [ H.text "Stop" ]
        , H.button { className: "btn btn-primary"
                  , on: {click: const $ setSelection CandidateTerm }
                  } [ H.text "Candidate" ]
      ]

toggleMaybe :: forall a. a -> Maybe a -> Maybe a
toggleMaybe _ (Just _) = Nothing
toggleMaybe b Nothing  = Just b

-- NEXT
data Action'
  = SetParentResetChildren' (Maybe NgramsTerm)
  | ToggleChild' (Maybe NgramsTerm) NgramsTerm
  | Synchronize'

-- NEXT
type Props =
  ( path         :: R.State PageParams
  , versioned    :: VersionedNgramsTable )

-- NEXT
loadedNgramsTable :: Record Props -> R.Element
loadedNgramsTable props = R.createElement loadedNgramsTableCpt props []

-- NEXT
loadedNgramsTableCpt :: R.Component Props
loadedNgramsTableCpt = R.hooksComponent "G.C.NgramsTable.loadedNgramsTable" cpt
  where
    cpt {versioned} _ = do
      state <- useNgramsReducer (initialState versioned)
      pure $ R.fragment []

    useNgramsReducer :: State -> R.Hooks (R.Reducer State Action')
    useNgramsReducer init = R2.useReductor' performNgramsAction init

    performNgramsAction :: Action' -> State -> Effect State
    performNgramsAction (SetParentResetChildren' term) = pure -- TODO
    performNgramsAction (ToggleChild' b c) = pure -- TODO
    performNgramsAction Synchronize' = pure -- TODO

type LoadedNgramsTableProps =
  ( path         :: R.State PageParams
  , tabNgramType :: CTabNgramType
  , versioned    :: VersionedNgramsTable
  , withAutoUpdate :: Boolean
  )

loadedNgramsTableSpec :: Thermite.Spec State (Record LoadedNgramsTableProps) Action
loadedNgramsTableSpec = Thermite.simpleSpec performAction render
  where
    setParentResetChildren :: Maybe NgramsTerm -> State -> State
    setParentResetChildren p = _ { ngramsParent = p, ngramsChildren = mempty }

    performAction :: Thermite.PerformAction State (Record LoadedNgramsTableProps) Action
    performAction (SetParentResetChildren p) _ _ =
      modifyState_ $ setParentResetChildren p
    performAction (ToggleChild b c) _ _ =
      modifyState_ $ _ngramsChildren <<< at c %~ toggleMaybe b
    performAction (ToggleSelect c) _ _ =
      modifyState_ $ _ngramsSelection <<< at c %~ toggleMaybe unit
    performAction ToggleSelectAll _ { ngramsSelectAll: true } =
      modifyState_ $ (_ngramsSelection .~ mempty)
                 <<< (_ngramsSelectAll .~ false)
    performAction ToggleSelectAll { versioned: Versioned { data: initTable } }
                                  state =
      let
        ngramsTable = applyNgramsPatches state initTable
        roots = rootsOf ngramsTable
      in
      modifyState_ $ (_ngramsSelection .~ roots)
                 <<< (_ngramsSelectAll .~ true)
    performAction Synchronize {path: path /\ _} state = do
      syncPatches path state
    performAction (CommitPatch pt) _ {ngramsVersion} =
      commitPatch (Versioned {version: ngramsVersion, data: pt})
    performAction ResetPatches _ {ngramsVersion} =
      modifyState_ $ \s -> s { ngramsLocalPatch = { ngramsNewElems: mempty, ngramsPatches: mempty } }
    performAction AddTermChildren _ {ngramsParent: Nothing} =
        -- impossible but harmless
        pure unit
    performAction AddTermChildren _
                  { ngramsParent: Just parent
                  , ngramsChildren
                  , ngramsVersion
                  } = do
        modifyState_ $ setParentResetChildren Nothing
        commitPatch (Versioned {version: ngramsVersion, data: pt})
      where
        pc = patchSetFromMap ngramsChildren
        pe = NgramsPatch { patch_list: mempty, patch_children: pc }
        pt = singletonNgramsTablePatch parent pe

    render :: Thermite.Render State (Record LoadedNgramsTableProps) Action
    render dispatch { path: path@({searchQuery, scoreType, params, termListFilter} /\ setPath)
                    , versioned: Versioned { data: initTable }
                    , tabNgramType
                    , withAutoUpdate }
                    state@{ ngramsParent, ngramsChildren, ngramsLocalPatch
                          , ngramsSelection, ngramsSelectAll }
                    _reactChildren =
      R2.scuff <$> (
        autoUpdate <> resetSaveButtons <> [
          T.table { colNames
                  , container
                  , params: params /\ setParams -- TODO-LENS
                  , rows: filteredRows
                  , totalRecords
                  , wrapColElts
                  }
        ]
      )
      where
        autoUpdate :: Array R.Element
        autoUpdate = if withAutoUpdate then [ R2.buff $ autoUpdateElt { duration: 5000, effect: dispatch Synchronize } ] else []
        resetButton :: R.Element
        resetButton = H.button { className: "btn btn-primary"
                               , on: { click: \_ -> dispatch ResetPatches } } [ H.text "Reset" ]
        saveButton :: R.Element
        saveButton = H.button { className: "btn btn-primary"
                              , on: { click: \_ -> dispatch Synchronize }} [ H.text "Save" ]
        resetSaveButtons :: Array R.Element
        resetSaveButtons = if ngramsLocalPatch == mempty then [] else
          [ H.div {} [ resetButton, saveButton ] ]

        totalRecords = A.length rows
        filteredRows = T.filterRows { params } rows
        colNames = T.ColumnName <$> ["Select", "Map", "Stop", "Terms", "Score"] -- see convOrderBy
        selected =
          H.input { checked: ngramsSelectAll
                  , className: "checkbox"
                  , on: { change: const $ dispatch $ ToggleSelectAll }
                  , type: "checkbox" }
        -- This is used to *decorate* the Select header with the checkbox.
        wrapColElts (T.ColumnName "Select") = const [selected]
        wrapColElts (T.ColumnName "Score")  = (_ <> [H.text ("(" <> show scoreType <> ")")])
        wrapColElts _                       = identity
        container = tableContainer { dispatch
                                   , ngramsChildren
                                   , ngramsParent
                                   , ngramsSelectAll
                                   , ngramsSelection
                                   , ngramsTable
                                   , path
                                   , tabNgramType
                                   }
        setParams f = setPath $ \p@{params: ps} -> p {params = f ps}
        ngramsTable = applyNgramsPatches state initTable
        orderWith =
          case convOrderBy <$> params.orderBy of
            Just ScoreAsc  -> A.sortWith \x -> (snd x)        ^. _NgramsElement <<< _occurrences
            Just ScoreDesc -> A.sortWith \x -> Down $ (snd x) ^. _NgramsElement <<< _occurrences
            _              -> identity -- the server ordering is enough here

        rows :: T.Rows
        rows = convertRow <$> orderWith (addOcc <$> Map.toUnfoldable (Map.filter displayRow (ngramsTable ^. _NgramsTable)))
        addOcc (Tuple ne ngramsElement) =
          let Additive occurrences = sumOccurrences ngramsTable ngramsElement in
          Tuple ne (ngramsElement # _NgramsElement <<< _occurrences .~ occurrences)

        ngramsParentRoot :: Maybe NgramsTerm
        ngramsParentRoot =
          (\np -> ngramsTable ^? at np
                            <<< _Just
                            <<< _NgramsElement
                            <<< _root
                            <<< _Just
            ) =<< ngramsParent

        displayRow (NgramsElement {ngrams, root, list}) =
          root == Nothing
          -- ^ Display only nodes without parents
          && ngramsChildren ^. at ngrams /= Just true
          -- ^ and which are not scheduled to be added already
          && Just ngrams /= ngramsParent
          -- ^ and which are not our new parent
          && Just ngrams /= ngramsParentRoot
          -- ^ and which are not the root of our new parent
          && queryMatchesLabel searchQuery (ngramsTermText ngrams)
          -- ^ and which matches the search query.
          && maybe true (_ == list) termListFilter
          -- ^ and which matches the ListType filter.
          || ngramsChildren ^. at ngrams == Just false
          -- ^ unless they are scheduled to be removed.
          || tablePatchHasNgrams ngramsLocalPatch ngrams
          -- ^ unless they are being processed at the moment.
        convertRow (Tuple ngrams ngramsElement) =
          { row: renderNgramsItem { dispatch
                                  , ngrams
                                  , ngramsElement
                                  , ngramsLocalPatch
                                  , ngramsParent
                                  , ngramsSelection
                                  , ngramsTable }
          , delete: false
          }

loadedNgramsTableClass :: ReactClass { children :: Children | LoadedNgramsTableProps }
loadedNgramsTableClass = Thermite.createClass "LoadedNgramsNgramsTable"
  loadedNgramsTableSpec (\{versioned} -> initialState versioned)

loadedNgramsTable' :: Record LoadedNgramsTableProps -> R.Element
loadedNgramsTable' props = R2.createElement' (loadedNgramsTableClass) props []

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
          Just (versioned :: VersionedNgramsTable) -> mainNgramsTablePaint {path, tabNgramType, versioned, withAutoUpdate}
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
      pure $ loadedNgramsTable' {
        path: pathS
      , tabNgramType
      , versioned
      , withAutoUpdate
      }

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
treeCpt = R.hooksComponent "G.C.NT.tree" cpt
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
              a (ngramsStyle <> [onClick $ const effect])
            Nothing ->
              span ngramsStyle
        edit effect = [ H.text " "
                      , H.i { className: "glyphicon glyphicon-pencil"
                            , on: { click: const effect } } []
                      ]
        leaf = List.null cs
        className = "glyphicon glyphicon-chevron-" <> if open then "down" else "right"
        style = if leaf then {color: "#adb5bd"} else {color: ""}
        open = not leaf || false {- TODO -}
        cs   = ngramsTable ^.. ix ngramsDepth.ngrams <<< _NgramsElement <<< _children <<< folded

        forest =
          let depth = ngramsDepth.depth + 1 in
          H.ul {} <<< map (\ngrams -> tree (params { ngramsDepth = {depth, ngrams} })) <<< List.toUnfoldable

sumOccurrences' :: NgramsTable -> NgramsTerm -> Additive Int
sumOccurrences' ngramsTable label =
    ngramsTable ^. ix label <<< to (sumOccurrences ngramsTable)

sumOccurrences :: NgramsTable -> NgramsElement -> Additive Int
sumOccurrences ngramsTable (NgramsElement {occurrences, children}) =
    Additive occurrences <> children ^. folded <<< to (sumOccurrences' ngramsTable)

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
renderNgramsTreeCpt = R.hooksComponent "G.C.NT.renderNgramsTree" cpt
  where
    cpt { ngramsTable, ngrams, ngramsStyle, ngramsClick, ngramsEdit } _ =
      pure $ H.ul {} [
        H.span { className: "tree" } [
          tree { ngramsClick
                , ngramsDepth: {ngrams, depth: 0}
                , ngramsEdit
                , ngramsStyle
                , ngramsTable
                }
        ]
      ]

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
renderNgramsItemCpt = R.hooksComponent "G.C.NT.renderNgramsItem" cpt
  where
    cpt { dispatch
        , ngrams
        , ngramsElement
        , ngramsLocalPatch
        , ngramsParent
        , ngramsSelection
        , ngramsTable } _ =
      pure $ T.makeRow [
          selected
        , checkbox GraphTerm
        , checkbox StopTerm
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
              termList'' = if chkd then CandidateTerm else termList'
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

tablePatchHasNgrams :: NgramsTablePatch -> NgramsTerm -> Boolean
tablePatchHasNgrams ngramsTablePatch ngrams =
  isJust $ ngramsTablePatch.ngramsPatches ^. _PatchMap <<< at ngrams

termStyle :: TermList -> Number -> DOM.Props
termStyle GraphTerm     opacity = style { color: "green", opacity}
termStyle StopTerm      opacity = style { color: "red",   opacity
                                        , textDecoration: "line-through"}
termStyle CandidateTerm opacity = style { color: "black", opacity}

nextTermList :: TermList -> TermList
nextTermList GraphTerm     = StopTerm
nextTermList StopTerm      = CandidateTerm
nextTermList CandidateTerm = GraphTerm

optps1 :: forall a. Show a => { desc :: String, mval :: Maybe a } -> R.Element
optps1 { desc, mval } = H.option { value: value } [H.text desc]
  where value = maybe "" show mval
