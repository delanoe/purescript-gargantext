module Gargantext.Components.NgramsTable
  ( MainNgramsTableProps
  , mainNgramsTable
  ) where

import Prelude
  ( class Show, Unit, bind, const, discard, identity, map, mempty, not
  , pure, show, unit, (#), ($), (&&), (+), (/=), (<$>), (<<<), (<>), (=<<)
  , (==), (||) )
import Data.Array as A
import Data.Lens (Lens', to, view, (%~), (.~), (^.), (^..))
import Data.Lens.Common (_Just)
import Data.Lens.At (at)
import Data.Lens.Index (ix)
import Data.Lens.Fold (folded)
import Data.Lens.Record (prop)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid.Additive (Additive(..))
import Data.Ord.Down (Down(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Reactix as R
import Reactix.DOM.HTML as H
import React (ReactElement)
import React.DOM (a, i, input, li, span, text, ul)
import React.DOM.Props (_type, checked, className, onChange, onClick, style)
import React.DOM.Props as DOM
import Thermite as Thermite
import Thermite (modifyState_)
import Gargantext.Types
  ( CTabNgramType, OrderBy(..), TabType, TermList(..), readTermList
  , readTermSize, termLists, termSizes)
import Gargantext.Components.AutoUpdate (autoUpdateElt)
import Gargantext.Components.NgramsTable.Core
  ( CoreState, LoadedNgramsTableProps, NgramsElement(..), NgramsPatch(..)
  , NgramsTable, NgramsTerm, PageParams, Replace(..), Versioned(..)
  , VersionedNgramsTable, _NgramsElement, _NgramsTable, _children
  , _list, _ngrams, _occurrences, _root, addNewNgram, applyNgramsTablePatch
  , applyPatchSet, commitPatch, convOrderBy, initialPageParams, loadNgramsTable
  , patchSetFromMap, replace, singletonNgramsTablePatch )
import Gargantext.Components.Table as T
import Gargantext.Hooks.Loader (useLoader2)
import Gargantext.Sessions (Session)
import Gargantext.Utils.Reactix as R2

type State =
  CoreState
  ( ngramsParent     :: Maybe NgramsTerm -- Nothing means we are not currently grouping terms
  , ngramsChildren   :: Map NgramsTerm Boolean
                     -- ^ Used only when grouping.
                     --   This updates the children of `ngramsParent`,
                     --   ngrams set to `true` are to be added, and `false` to
                     --   be removed.
  )

_ngramsChildren :: forall row. Lens' { ngramsChildren :: Map NgramsTerm Boolean | row } (Map NgramsTerm Boolean)
_ngramsChildren = prop (SProxy :: SProxy "ngramsChildren")

initialState :: VersionedNgramsTable -> State
initialState (Versioned {version}) =
  { ngramsTablePatch: mempty
  , ngramsVersion:    version
  , ngramsParent:     Nothing
  , ngramsChildren:   mempty
  }

data Action
  = SetTermListItem NgramsTerm (Replace TermList)
  | SetParentResetChildren (Maybe NgramsTerm)
  -- ^ This sets `ngramsParent` and resets `ngramsChildren`.
  | ToggleChild Boolean NgramsTerm
  -- ^ Toggles the NgramsTerm in the `PatchSet` `ngramsChildren`.
  -- If the `Boolean` is `true` it means we want to add it if it is not here,
  -- if it is `false` it is meant to be removed if not here.
  | AddTermChildren -- NgramsTable
  -- ^ The NgramsTable argument is here as a cache of `ngramsTablePatch`
  -- applied to `initTable`.
  -- TODO more docs
  | Refresh
  | AddNewNgram NgramsTerm

type Dispatch = Action -> Effect Unit

tableContainer :: { pageParams     :: PageParams
                  , dispatch       :: Dispatch
                  , setPath        :: R2.Setter PageParams
                  , ngramsParent   :: Maybe NgramsTerm
                  , ngramsChildren :: Map NgramsTerm Boolean
                  , ngramsTable    :: NgramsTable
                  }
               -> Record T.TableContainerProps -> R.Element
tableContainer { pageParams
               , dispatch
               , setPath
               , ngramsParent
               , ngramsChildren
               , ngramsTable: ngramsTableCache
               } props =
  H.div {className: "container-fluid"}
  [ H.div {className: "jumbotron1"}
    [ H.div {className: "row"}
      [ H.div {className: "panel panel-default"}
        [ H.div {className: "panel-heading"}
          [ H.h2 {className: "panel-title", style: {textAlign : "center"}}
            [ H.span {className: "glyphicon glyphicon-hand-down"} []
            , H.text "Extracted Terms"
            ]
          , H.div {className: "row"}
            [ H.div {className: "col-md-3", style: {marginTop: "6px"}}
              [ H.input { className: "form-control"
                        , name: "search"
                        , placeholder: "Search"
                        , type: "value"
                        , value: pageParams.searchQuery
                        , on: {input: \e -> setSearchQuery (R2.unsafeEventValue e)}}
              , H.div {} (
                   if A.null props.tableBody && pageParams.searchQuery /= "" then [
                     H.button { className: "btn btn-primary"
                              , on: {click: const $ dispatch $ AddNewNgram pageParams.searchQuery}}
                     [ H.text ("Add " <> pageParams.searchQuery) ]
                     ] else [])]
            , H.div {className: "col-md-2", style: {marginTop : "6px"}}
              [ H.li {className: " list-group-item"}
                [ R2.select { id: "picklistmenu"
                            , className: "form-control custom-select"
                            , value: (maybe "" show pageParams.termListFilter)
                            , on: {change: (\e -> setTermListFilter $ readTermList $ R2.unsafeEventValue e)}}
                  (map optps1 termLists)]]
            , H.div {className: "col-md-2", style: {marginTop : "6px"}}
              [ H.li {className: "list-group-item"}
                [ R2.select {id: "picktermtype"
                            , className: "form-control custom-select"
                            , value: (maybe "" show pageParams.termSizeFilter)
                            , on: {change: (\e -> setTermSizeFilter $ readTermSize $ R2.unsafeEventValue e)}}
                    (map optps1 termSizes)]]
            , H.div {className: "col-md-4", style: {marginTop : "6px", marginBottom : "1px"}}
              [ H.li {className: " list-group-item"}
                [ props.pageSizeDescription
                , props.pageSizeControl
                , H.text " items / "
                , props.paginationLinks]]]]
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
                ngramsEdit _ = Nothing
              in
              [ H.p {} [H.text $ "Editing " <> ngrams]
              , R2.buff $ renderNgramsTree { ngramsTable, ngrams, ngramsStyle: [], ngramsClick, ngramsEdit }
              , H.button {className: "btn btn-primary", on: {click: (const $ dispatch $ AddTermChildren)}} [H.text "Save"]
              , H.button {className: "btn btn-secondary", on: {click: (const $ dispatch $ SetParentResetChildren Nothing)}} [H.text "Cancel"]
              ]) ngramsParent)
          , H.div {id: "terms_table", className: "panel-body"}
            [ H.table {className: "table able"}
              [ H.thead {className: "tableHeader"} [props.tableHead]
              , H.tbody {} props.tableBody]]]]]]
  where
    setPageParams f = setPath (const $ f pageParams)
    setSearchQuery    x = setPageParams $ _ { searchQuery = x }
    setTermListFilter x = setPageParams $ _ { termListFilter = x }
    setTermSizeFilter x = setPageParams $ _ { termSizeFilter = x }

toggleMap :: forall a. a -> Maybe a -> Maybe a
toggleMap _ (Just _) = Nothing
toggleMap b Nothing  = Just b

data Action'
  = SetParentResetChildren' (Maybe NgramsTerm)
  | ToggleChild' (Maybe NgramsTerm) NgramsTerm
  | Refresh'

type Props =
  ( session      :: Session
  , tabNgramType :: CTabNgramType
  , path         :: R.State PageParams
  , versioned    :: VersionedNgramsTable )

loadedNgramsTable :: Record Props -> R.Element
loadedNgramsTable props = R.createElement loadedNgramsTableCpt props []

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
    performNgramsAction Refresh' = pure -- TODO

ngramsTableSpec :: Session -> CTabNgramType -> R2.Setter PageParams -> Thermite.Spec State (Record LoadedNgramsTableProps) Action
ngramsTableSpec session ntype setPath = Thermite.simpleSpec performAction render
  where
    setParentResetChildren :: Maybe NgramsTerm -> State -> State
    setParentResetChildren p = _ { ngramsParent = p, ngramsChildren = mempty }

    performAction :: Thermite.PerformAction State (Record LoadedNgramsTableProps) Action
    performAction (SetParentResetChildren p) _ _ =
      modifyState_ $ setParentResetChildren p
    performAction (ToggleChild b c) _ _ =
      modifyState_ $ _ngramsChildren <<< at c %~ toggleMap b
    performAction Refresh {path: {nodeId, listIds, tabType}} {ngramsVersion} = do
        commitPatch session {nodeId, listIds, tabType} (Versioned {version: ngramsVersion, data: mempty})
    performAction (SetTermListItem n pl) {path: {nodeId, listIds, tabType}} {ngramsVersion} =
        commitPatch session {nodeId, listIds, tabType} (Versioned {version: ngramsVersion, data: pt})
      where
        pe = NgramsPatch { patch_list: pl, patch_children: mempty }
        pt = singletonNgramsTablePatch ntype n pe
    performAction AddTermChildren _ {ngramsParent: Nothing} =
        -- impossible but harmless
        pure unit
    performAction AddTermChildren {path: {nodeId, listIds, tabType}}
                  { ngramsParent: Just parent
                  , ngramsChildren
                  , ngramsVersion
                  } = do
        modifyState_ $ setParentResetChildren Nothing
        commitPatch session {nodeId, listIds, tabType} (Versioned {version: ngramsVersion, data: pt})
      where
        pc = patchSetFromMap ngramsChildren
        pe = NgramsPatch { patch_list: mempty, patch_children: pc }
        pt = singletonNgramsTablePatch ntype parent pe
    performAction (AddNewNgram ngram) {path: {listIds, nodeId, tabType}} {ngramsVersion} =
        commitPatch session {listIds, nodeId, tabType} (Versioned {version: ngramsVersion, data: pt})
      where
        pt = addNewNgram ntype ngram CandidateTerm

    render :: Thermite.Render State (Record LoadedNgramsTableProps) Action
    render dispatch { path: pageParams
                    , loaded: Versioned { data: initTable } }
                    { ngramsTablePatch, ngramsParent, ngramsChildren }
                    _reactChildren =
      [ autoUpdateElt { duration: 3000, effect: dispatch Refresh }
      , R2.scuff $ T.table { rows, params, container, colNames, totalRecords}
      ]
      where
        totalRecords = 47361 -- TODO
        colNames = T.ColumnName <$> ["Map", "Stop", "Terms", "Score (Occurrences)"] -- see convOrderBy
        container = tableContainer {pageParams, setPath, dispatch, ngramsParent, ngramsChildren, ngramsTable}
        setParams f = setPath $ \p@{params: ps} -> p {params = f ps}
        params = pageParams.params /\ setParams
        ngramsTable = applyNgramsTablePatch ngramsTablePatch initTable
        orderWith =
          case convOrderBy <$> pageParams.params.orderBy of
            Just ScoreAsc  -> A.sortWith \x -> (snd x) ^. _NgramsElement <<< _occurrences
            Just ScoreDesc -> A.sortWith \x -> Down $ (snd x) ^. _NgramsElement <<< _occurrences
            _              -> identity -- the server ordering is enough here

        rows = convertRow <$> orderWith (addOcc <$> Map.toUnfoldable (Map.filter displayRow (ngramsTable ^. _NgramsTable)))
        addOcc (Tuple ne ngramsElement) =
          let Additive occurrences = sumOccurrences ngramsTable ngramsElement in
          Tuple ne (ngramsElement # _NgramsElement <<< _occurrences .~ occurrences)

        ngramsParentRoot :: Maybe String
        ngramsParentRoot =
          (\np -> ngramsTable ^. at np <<< _Just <<< _NgramsElement <<< _root) =<< ngramsParent

        displayRow (NgramsElement {ngrams, root}) =
          root == Nothing
          -- ^ Display only nodes without parents
          && ngramsChildren ^. at ngrams /= Just true
          -- ^ and which are not scheduled to be added already
          && Just ngrams /= ngramsParent
          -- ^ and which are not our new parent
          && Just ngrams /= ngramsParentRoot
          -- ^ and which are not the root of our new parent
          || -- Unless they are scheduled to be removed.
          ngramsChildren ^. at ngrams == Just false
        convertRow (Tuple ngrams ngramsElement) =
          { row: R2.buff <$> renderNgramsItem { ngramsTable, ngrams, ngramsParent, ngramsElement, dispatch}
          , delete: false
          }

-- ngramsTableClass :: Session -> CTabNgramType -> R2.Setter PageParams -> Loader.InnerClass PageParams (Versioned NgramsTable)
-- ngramsTableClass session ct setPath = createClass "NgramsTable" (ngramsTableSpec session ct setPath) initialState

-- ngramsTable' :: Session -> CTabNgramType -> R2.Setter PageParams -> Record LoadedNgramsTableProps -> R.Element
-- ngramsTable' session ct setPath props = R2.createElement' (ngramsTableClass session ct setPath) props []

type MainNgramsTableProps =
  ( nodeId        :: Int
    -- ^ This node can be a corpus or contact.
  , defaultListId :: Int
  , tabType       :: TabType
  , session          :: Session
  , tabNgramType  :: CTabNgramType
  )

mainNgramsTable :: Record MainNgramsTableProps -> R.Element
mainNgramsTable props = R.createElement mainNgramsTableCpt props []

mainNgramsTableCpt :: R.Component MainNgramsTableProps
mainNgramsTableCpt = R.hooksComponent "MainNgramsTable" cpt
  where
    cpt {nodeId, defaultListId, tabType, session, tabNgramType} _ = do
      path <- R.useState' $ initialPageParams session nodeId [defaultListId] tabType
      useLoader2 path (loadNgramsTable session) $
        \versioned -> loadedNgramsTable {session, tabNgramType, path, versioned}

type NgramsDepth = {ngrams :: NgramsTerm, depth :: Int}
type NgramsClick = NgramsDepth -> Maybe (Effect Unit)

tree :: { ngramsTable :: NgramsTable
        , ngramsStyle :: Array DOM.Props
        , ngramsEdit  :: NgramsClick
        , ngramsClick :: NgramsClick
        } -> NgramsDepth -> ReactElement
tree params@{ngramsTable, ngramsStyle, ngramsEdit, ngramsClick} nd =
  li [ style {width : "100%"} ]
    ([ i icon []
     , tag [text $ " " <> nd.ngrams]
     ] <> maybe [] edit (ngramsEdit nd) <>
     [ forest cs
     ])
  where
    tag =
      case ngramsClick nd of
        Just effect ->
          a (ngramsStyle <> [onClick $ const effect])
        Nothing ->
          span ngramsStyle
    edit effect = [ text " "
                  , i [ className "glyphicon glyphicon-pencil"
                      , onClick $ const effect ] [] ]
    leaf = List.null cs
    icon = gray <> [className $ "glyphicon glyphicon-chevron-" <> if open then "down" else "right"]
    open = not leaf || false {- TODO -}
    gray = if leaf then [style {color: "#adb5bd"}] else []
    cs   = ngramsTable ^.. ix nd.ngrams <<< _NgramsElement <<< _children <<< folded

    forest =
      let depth = nd.depth + 1 in
      ul [] <<< map (\ngrams -> tree params {depth, ngrams}) <<< List.toUnfoldable

sumOccurrences' :: NgramsTable -> NgramsTerm -> Additive Int
sumOccurrences' ngramsTable label =
    ngramsTable ^. ix label <<< to (sumOccurrences ngramsTable)

sumOccurrences :: NgramsTable -> NgramsElement -> Additive Int
sumOccurrences ngramsTable (NgramsElement {occurrences, children}) =
    Additive occurrences <> children ^. folded <<< to (sumOccurrences' ngramsTable)

renderNgramsTree :: { ngrams      :: NgramsTerm
                    , ngramsTable :: NgramsTable
                    , ngramsStyle :: Array DOM.Props
                    , ngramsClick :: NgramsClick
                    , ngramsEdit  :: NgramsClick
                    } -> ReactElement
renderNgramsTree { ngramsTable, ngrams, ngramsStyle, ngramsClick, ngramsEdit } =
  ul [] [
    span [className "tree"] [tree {ngramsTable, ngramsStyle, ngramsClick, ngramsEdit} {ngrams, depth: 0}]
  ]

renderNgramsItem :: { ngrams :: NgramsTerm
                    , ngramsTable :: NgramsTable
                    , ngramsElement :: NgramsElement
                    , ngramsParent :: Maybe NgramsTerm
                    , dispatch :: Action -> Effect Unit
                    } -> Array ReactElement
renderNgramsItem { ngramsTable, ngrams, ngramsElement, ngramsParent, dispatch } =
  [ checkbox GraphTerm
  , checkbox StopTerm
  , if ngramsParent == Nothing
    then renderNgramsTree { ngramsTable, ngrams, ngramsStyle, ngramsClick, ngramsEdit }
    else
      a [onClick $ const $ dispatch $ ToggleChild true ngrams]
        [ i [className "glyphicon glyphicon-plus"] []
        , span ngramsStyle [text $ " " <> ngrams]
        ]
  , text $ show (ngramsElement ^. _NgramsElement <<< _occurrences)
  ]
  where
    termList    = ngramsElement ^. _NgramsElement <<< _list
    ngramsStyle = [termStyle termList]
    ngramsEdit  = Just <<< dispatch <<< SetParentResetChildren <<< Just <<< view _ngrams
    ngramsClick = Just <<< cycleTermListItem <<< view _ngrams
    checkbox termList' =
      let chkd = termList == termList'
          termList'' = if chkd then CandidateTerm else termList'
      in
      input
        [ _type "checkbox"
        , className "checkbox"
        , checked chkd
     -- , title "Mark as completed"
        , onChange $ const $ setTermList (replace termList termList'') ngrams
        ]

    setTermList Keep                    _ = pure unit
    setTermList rep@(Replace {old,new}) n = dispatch $ SetTermListItem n rep

    cycleTermListItem = setTermList (replace termList (nextTermList termList))

termStyle :: TermList -> DOM.Props
termStyle GraphTerm     = style {color: "green"}
termStyle StopTerm      = style {color: "red", textDecoration : "line-through"}
termStyle CandidateTerm = style {color: "black"}

nextTermList :: TermList -> TermList
nextTermList GraphTerm     = StopTerm
nextTermList StopTerm      = CandidateTerm
nextTermList CandidateTerm = GraphTerm

optps1 :: forall a. Show a => { desc :: String, mval :: Maybe a } -> R.Element
optps1 { desc, mval } = H.option {value} [H.text desc]
  where value = maybe "" show mval
