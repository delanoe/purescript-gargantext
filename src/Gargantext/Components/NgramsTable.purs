module Gargantext.Components.NgramsTable
  ( Action
  , MainNgramsTableProps
  , initialState
  , mainNgramsTableSpec
  , ngramsTableClass
  , ngramsTableSpec
  , termStyle
  )
  where

import Control.Monad.Cont.Trans (lift)
import Data.Array as A
import Data.Lens (to, view, (%~), (.~), (^.), (^..))
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
import Effect (Effect)
import React (ReactElement)
import React.DOM (a, button, div, h2, i, input, li, option, p, select, span, table, tbody, text, thead, ul)
import React.DOM.Props (_id, _type, checked, className, name, onChange, onClick, onInput, placeholder, style, value)
import React.DOM.Props as DOM
import Thermite (PerformAction, Render, Spec, defaultPerformAction, modifyState_, simpleSpec, createClass)
import Unsafe.Coerce (unsafeCoerce)

import Gargantext.Types (TermList(..), readTermList, readTermSize, termLists, termSizes)
import Gargantext.Config (OrderBy(..), TabType)
import Gargantext.Components.AutoUpdate (autoUpdateElt)
import Gargantext.Components.Table as T
import Gargantext.Prelude
import Gargantext.Components.Loader as Loader
import Gargantext.Components.NgramsTable.Core

type State =
  CoreState
  ( ngramsParent     :: Maybe NgramsTerm -- Nothing means we are not currently grouping terms
  , ngramsChildren   :: Map NgramsTerm Boolean
                     -- ^ Used only when grouping.
                     --   This updates the children of `ngramsParent`,
                     --   ngrams set to `true` are to be added, and `false` to
                     --   be removed.
  )

_ngramsChildren = prop (SProxy :: SProxy "ngramsChildren")

initialState :: forall props. { loaded :: VersionedNgramsTable | props }
             -> State
initialState {loaded: Versioned {version}} =
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

type LoaderAction = Loader.Action PageParams

type LoaderDispatch = LoaderAction -> Effect Unit

tableContainer :: { pageParams :: PageParams
                  , dispatch :: Dispatch
                  , loaderDispatch :: LoaderDispatch
                  , ngramsParent :: Maybe NgramsTerm
                  , ngramsChildren :: Map NgramsTerm Boolean
                  , ngramsTable :: NgramsTable
                  }
               -> T.TableContainerProps -> Array ReactElement
tableContainer { pageParams
               , dispatch
               , loaderDispatch
               , ngramsParent
               , ngramsChildren
               , ngramsTable: ngramsTableCache
               } props =
  [ div [className "container-fluid"]
    [ div [className "jumbotron1"]
      [ div [className "row"]
        [ div [className "panel panel-default"]
          [ div [className "panel-heading"]
            [ h2 [className "panel-title", style {textAlign : "center"}]
              [ span [className "glyphicon glyphicon-hand-down"] []
              , text "Extracted Terms"
              ]
            , div [className "row"]
              [
              {-div [className "savediv pull-left col-md-2", style { marginTop :"35px"}]
                [  button [_id "ImportListOrSaveAll", className "btn btn-warning", style {fontSize : "120%"}]
                  [ text "Import a Termlist" ]
                ]
              ,-}
                div [className "col-md-3", style {marginTop : "6px"}]
                [ input [ className "form-control "
                        , name "search", placeholder "Search"
                        , _type "value"
                        , value pageParams.searchQuery
                        , onInput \e -> setSearchQuery (unsafeEventValue e)
                        ]
                , div [] (
                    if A.null props.tableBody && pageParams.searchQuery /= "" then [
                      button [ className "btn btn-primary"
                             , onClick $ const $ dispatch $ AddNewNgram pageParams.searchQuery
                             ] [text $ "Add " <> pageParams.searchQuery]
                    ] else []
                  )
                ]
              , div [className "col-md-2", style {marginTop : "6px"}]
                      [ li [className " list-group-item"]
                        [ select  [ _id "picklistmenu"
                                  , className "form-control custom-select"
                                  , value (maybe "" show pageParams.termListFilter)
                                  , onChange (\e -> setTermListFilter $ readTermList $ unsafeEventValue e)
                                  ] $ map optps1 termLists
                        ]
                      ]
              , div [className "col-md-2", style {marginTop : "6px"}]
                      [ li [className "list-group-item"]
                        [ select  [ _id "picktermtype"
                                  , className "form-control custom-select"
                                  , value (maybe "" show pageParams.termSizeFilter)
                                  , onChange (\e -> setTermSizeFilter $ readTermSize $ unsafeEventValue e)
                                  ] $ map optps1 termSizes
                        ]
                      ]

              , div [className "col-md-4", style {marginTop : "6px", marginBottom : "1px"}]
                [ li [className " list-group-item"] [ props.pageSizeDescription
                                                    , props.pageSizeControl
                                                    , text " items / "
                                                    , props.paginationLinks
                                                    ]
                --, li [className " list-group-item"] [ props.pageSizeControl ]
                ]
              ]
            ]
          , div [] (maybe [] (\ngrams ->
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
              in
              [ p[] [text $ "Editing " <> ngrams]
              , renderNgramsTree { ngramsTable, ngrams, ngramsStyle: [], ngramsClick }
              , button [className "btn btn-primary", onClick $ const $ dispatch $ AddTermChildren] [text "Save"]
              , button [className "btn btn-secondary", onClick $ const $ dispatch $ SetParentResetChildren Nothing] [text "Cancel"]
              ]) ngramsParent)
          , div [ _id "terms_table", className "panel-body" ]
                [ table [ className "table able" ]
                  [ thead [ className "tableHeader"] [props.tableHead]
                  , tbody [] props.tableBody
                  ]
                ]
          ]
        ]
      ]
    ]
  ]
  where
    setPageParams f = loaderDispatch $ Loader.SetPath $ f pageParams
    setSearchQuery    x = setPageParams $ _ { searchQuery = x }
    setTermListFilter x = setPageParams $ _ { termListFilter = x }
    setTermSizeFilter x = setPageParams $ _ { termSizeFilter = x }

toggleMap :: forall a. a -> Maybe a -> Maybe a
toggleMap _ (Just _) = Nothing
toggleMap b Nothing  = Just b

ngramsTableSpec :: Spec State LoadedNgramsTableProps Action
ngramsTableSpec = simpleSpec performAction render
  where
    setParentResetChildren :: Maybe NgramsTerm -> State -> State
    setParentResetChildren p = _ { ngramsParent = p, ngramsChildren = mempty }

    performAction :: PerformAction State LoadedNgramsTableProps Action
    performAction (SetParentResetChildren p) _ _ =
      modifyState_ $ setParentResetChildren p
    performAction (ToggleChild b c) _ _ =
      modifyState_ $ _ngramsChildren <<< at c %~ toggleMap b
    performAction Refresh {path: {nodeId, listIds, tabType}} {ngramsVersion} = do
        commitPatch {nodeId, listIds, tabType} (Versioned {version: ngramsVersion, data: mempty})
    performAction (SetTermListItem n pl) {path: {nodeId, listIds, tabType}} {ngramsVersion} =
        commitPatch {nodeId, listIds, tabType} (Versioned {version: ngramsVersion, data: pt})
      where
        listId = Just 10 -- List.head listIds
        pe = NgramsPatch { patch_list: pl, patch_children: mempty }
        pt = PatchMap $ Map.singleton n pe
    performAction AddTermChildren _ {ngramsParent: Nothing} =
        -- impossible but harmless
        pure unit
    performAction AddTermChildren {path: {nodeId, listIds, tabType}}
                  { ngramsParent: Just parent
                  , ngramsChildren
                  , ngramsVersion
                  } = do
        modifyState_ $ setParentResetChildren Nothing
        commitPatch {nodeId, listIds, tabType} (Versioned {version: ngramsVersion, data: pt})
      where
        listId = Just 10 -- List.head listIds
        pc = patchSetFromMap ngramsChildren
        pe = NgramsPatch { patch_list: mempty, patch_children: pc }
        pt = PatchMap $ Map.fromFoldable [Tuple parent pe]
        -- TODO ROOT-UPDATE
        -- patch the root of the child to be equal to the root of the parent.
    performAction (AddNewNgram ngram) {path: params} _ =
      lift $ addNewNgram ngram Nothing params

    render :: Render State LoadedNgramsTableProps Action
    render dispatch { path: pageParams
                    , loaded: Versioned { data: initTable }
                    , dispatch: loaderDispatch }
                    { ngramsTablePatch, ngramsParent, ngramsChildren }
                    _reactChildren =
      [ autoUpdateElt { duration: 3000
                      , effect:   dispatch Refresh
                      }
      , T.tableElt
          { rows
          , setParams
          , container: tableContainer {pageParams, loaderDispatch, dispatch, ngramsParent, ngramsChildren, ngramsTable}
          , colNames:
              T.ColumnName <$>
              [ "Graph"
              , "Stop"
              , "Terms"
              , "Score (Occurrences)" -- see convOrderBy
              ]
          , totalRecords: 47361 -- TODO
          }
      ]
          where
            setParams params =
              loaderDispatch $ Loader.SetPath $ pageParams {params = params}
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
              { row: renderNgramsItem { ngramsTable, ngrams, ngramsParent, ngramsElement, dispatch}
              , delete: false
              }

ngramsTableClass :: Loader.InnerClass PageParams VersionedNgramsTable
ngramsTableClass = createClass "NgramsTable" ngramsTableSpec initialState

type MainNgramsTableProps =
  Loader.InnerProps Int { defaultListId :: Int }
                        ( tabType :: TabType )

mainNgramsTableSpec :: Spec {} MainNgramsTableProps Void
mainNgramsTableSpec = simpleSpec defaultPerformAction render
  where
    render :: Render {} MainNgramsTableProps Void
    render _ {path: nodeId, loaded: {defaultListId}, tabType} _ _ =
      [ ngramsLoader
          { path: initialPageParams nodeId [defaultListId] tabType
          , component: ngramsTableClass
          } ]

type NgramsDepth = {ngrams :: NgramsTerm, depth :: Int}
type NgramsClick = NgramsDepth -> Maybe (Effect Unit)

tree :: { ngramsTable :: NgramsTable
        , ngramsStyle :: Array DOM.Props
        , ngramsClick :: NgramsClick
        } -> NgramsDepth -> ReactElement
tree params@{ngramsTable, ngramsStyle, ngramsClick} nd@{ngrams} =
  li [ style {width : "100%"} ]
     [ i icon []
     , tag [text $ " " <> ngrams]
     , forest cs
     ]
  where
    tag =
      case ngramsClick nd of
        Just effect ->
          a (ngramsStyle <> [onClick $ const effect])
        Nothing ->
          span ngramsStyle
    leaf = List.null cs
    icon = gray <> [className $ "fas fa-caret-" <> if open then "down" else "right"]
    open = not leaf || false {- TODO -}
    gray = if leaf then [style {color: "#adb5bd"}] else []
    cs   = ngramsTable ^.. ix ngrams <<< _NgramsElement <<< _children <<< folded

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
                    } -> ReactElement
renderNgramsTree { ngramsTable, ngrams, ngramsStyle, ngramsClick } =
  ul [] [
    span [className "tree"] [tree {ngramsTable, ngramsStyle, ngramsClick} {ngrams, depth: 0}]
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
    then renderNgramsTree { ngramsTable, ngrams, ngramsStyle, ngramsClick }
    else
      a [onClick $ const $ dispatch $ ToggleChild true ngrams]
        [ i [className "fas fa-plus"] []
        , span ngramsStyle [text $ " " <> ngrams]
        ]
  , text $ show (ngramsElement ^. _NgramsElement <<< _occurrences)
  ]
  where
    termList    = ngramsElement ^. _NgramsElement <<< _list
    ngramsStyle = [termStyle termList]
    ngramsClick = Just <<< dispatch <<< SetParentResetChildren <<< Just <<< view _ngrams
    checkbox termList' =
      let chkd = termList == termList'
          termList'' = if chkd then CandidateTerm else termList'
      in
      input
        [ _type "checkbox"
        , className "checkbox"
        , checked chkd
     -- , title "Mark as completed"
        , onChange $ const $ setTermList (replace termList termList'')
        ]

    setTermList Keep = pure unit
    setTermList rep@(Replace {old,new}) = dispatch $ SetTermListItem ngrams rep

termStyle :: TermList -> DOM.Props
termStyle GraphTerm     = style {color: "green"}
termStyle StopTerm      = style {color: "red", textDecoration : "line-through"}
termStyle CandidateTerm = style {color: "black"}

optps1 :: forall a. Show a => { desc :: String, mval :: Maybe a } -> ReactElement
optps1 { desc, mval } = option [value val] [text desc]
  where
    val = maybe "" show mval

unsafeEventValue :: forall event. event -> String
unsafeEventValue e = (unsafeCoerce e).target.value
