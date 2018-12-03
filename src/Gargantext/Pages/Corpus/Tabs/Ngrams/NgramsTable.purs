module Gargantext.Pages.Corpus.Tabs.Ngrams.NgramsTable where


import Control.Monad.State (class MonadState, execState)
import Data.Argonaut (class DecodeJson, decodeJson, (.?), (.??))
import Data.Array (filter, toUnfoldable)
import Data.Either (Either(..))
import Data.Foldable
import Data.FoldableWithIndex
import Data.FunctorWithIndex
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, unwrap)
import Data.Lens (Lens', Prism', Iso', lens, over, prism, (^.), (^..), (%~), (.=), use, (<>~))
import Data.Lens.Common (_Just)
import Data.Lens.At (class At, at)
import Data.Lens.Index (class Index, ix)
import Data.Lens.Fold (folded)
import Data.Lens.Getter (to)
import Data.Lens.Record (prop)
import Data.Lens.Iso (re)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (class Traversable, traverse, traverse_, sequence)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..), uncurry)
import Data.Void (Void)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff (Aff)
import React (ReactElement, ReactClass, Children)
import React as React
import React.DOM hiding (style, map)
import React.DOM.Props (_id, _type, checked, className, href, name, onChange, onClick, onInput, placeholder, scope, selected, style, value)
import React.DOM.Props as DOM
import Thermite (PerformAction, Spec, StateCoTransformer, Render, _render, modifyState_, defaultPerformAction, focusState, hideState, simpleSpec, createClass)
import Unsafe.Coerce (unsafeCoerce)

import Gargantext.Types
import Gargantext.Components.Table as T
import Gargantext.Prelude
import Gargantext.Config
import Gargantext.Config.REST
import Gargantext.Components.Loader as Loader
import Gargantext.Pages.Corpus.Tabs.Types (CorpusInfo(..), PropsRow)

type Props = { mode :: Mode | PropsRow }

type PageParams = {nodeId :: Int, params :: T.Params, mode :: Mode}

type Props' = Loader.InnerProps PageParams NgramsTable ()

type NgramsTerm = String

newtype NgramsElement = NgramsElement
  { ngrams      :: NgramsTerm
  , list        :: TermList
  , occurrences :: Int
  , parent      :: Maybe NgramsTerm
  , children    :: Set NgramsTerm
  }

_parent = prop (SProxy :: SProxy "parent")
_children :: forall row. Lens' { children :: Set NgramsTerm | row } (Set NgramsTerm)
_children = prop (SProxy :: SProxy "children")

derive instance newtypeNgramsElement :: Newtype NgramsElement _

_NgramsElement :: Iso' NgramsElement _
_NgramsElement = _Newtype

instance decodeJsonNgramsElement :: DecodeJson NgramsElement where
  decodeJson json = do
    obj         <- decodeJson json
    ngrams      <- obj .?  "ngrams"
    list        <- obj .?  "list"
    occurrences <- obj .?  "occurrences"
    parent      <- obj .?? "parent"
    children'   <- obj .?  "children"
    let children = Set.fromFoldable (children' :: Array NgramsTerm)
    pure $ NgramsElement {ngrams, list, occurrences, parent, children}

-- type NgramsTable = Array (NTree NgramsElement)
-- type NgramsTable = Array NgramsElement
newtype NgramsTable = NgramsTable (Map NgramsTerm NgramsElement)

derive instance newtypeNgramsTable :: Newtype NgramsTable _

_NgramsTable :: Iso' NgramsTable (Map NgramsTerm NgramsElement)
_NgramsTable = _Newtype

instance indexNgramsTable :: Index NgramsTable String NgramsElement where
  ix k = _NgramsTable <<< ix k

instance atNgramsTable :: At NgramsTable String NgramsElement where
  at k = _NgramsTable <<< at k

instance decodeJsonNgramsTable :: DecodeJson NgramsTable where
  decodeJson json = do
    elements <- decodeJson json
    pure $ NgramsTable
         $ Map.fromFoldable
         $ f <$> (elements :: Array NgramsElement)
    where
      f e@(NgramsElement e') = Tuple e'.ngrams e

data Replace a
  = Keep
  | Replace { old :: a, new :: a }

replace :: forall a. Eq a => a -> a -> Replace a
replace old new
  | old == new = Keep
  | otherwise  = Replace { old, new }

instance semigroupReplace :: Semigroup (Replace a) where
  append Keep p = p
  append p Keep = p
  append (Replace { old: _m, new }) (Replace { old, new: _m' }) =
    -- assert _m == _m'
    Replace { old, new }

instance semigroupMonoid :: Monoid (Replace a) where
  mempty = Keep

applyReplace :: forall a. Eq a => Replace a -> a -> a
applyReplace Keep a = a
applyReplace (Replace { old, new }) a
  | a == old  = new
  | otherwise = a

-- Representing a PatchSet as `Map a Boolean` would have the advantage
-- of enforcing rem and add to be disjoint.
newtype PatchSet a = PatchSet
  { rem :: Set a
  , add :: Set a
  }

instance semigroupPatchSet :: Ord a => Semigroup (PatchSet a) where
  append (PatchSet p) (PatchSet q) = PatchSet
    { rem: q.rem <> p.rem
    , add: Set.difference q.add p.rem <> p.add
    }

instance monoidPatchSet :: Ord a => Monoid (PatchSet a) where
  mempty = PatchSet { rem: Set.empty, add: Set.empty }

applyPatchSet :: forall a. Ord a => PatchSet a -> Set a -> Set a
applyPatchSet (PatchSet p) s = Set.difference s p.rem <> p.add

patchSetFromMap :: forall a. Ord a => Map a Boolean -> PatchSet a
patchSetFromMap m = PatchSet { rem: Map.keys (Map.filter not m)
                             , add: Map.keys (Map.filter identity m) }
  -- TODO Map.partition would be nice here

newtype NgramsPatch = NgramsPatch
  { patch_children :: PatchSet NgramsTerm
  , patch_list     :: Replace TermList
  }

instance semigroupNgramsPatch :: Semigroup NgramsPatch where
  append (NgramsPatch p) (NgramsPatch q) = NgramsPatch
    { patch_children: p.patch_children <> q.patch_children
    , patch_list:     p.patch_list     <> q.patch_list
    }

instance monoidNgramsPatch :: Monoid NgramsPatch where
  mempty = NgramsPatch { patch_children: mempty, patch_list: mempty }

applyNgramsPatch :: NgramsPatch -> NgramsElement -> NgramsElement
applyNgramsPatch (NgramsPatch p) (NgramsElement e) = NgramsElement
  { ngrams:      e.ngrams
  , list:        applyReplace p.patch_list e.list
  , occurrences: e.occurrences -- TODO: is this correct ?
  , parent:      e.parent
  , children:    applyPatchSet p.patch_children e.children
  }

newtype PatchMap k p = PatchMap (Map k p)

instance semigroupPatchMap :: (Ord k, Semigroup p) => Semigroup (PatchMap k p) where
  append (PatchMap p) (PatchMap q) = PatchMap (Map.unionWith append p q)

instance monoidPatchMap :: (Ord k, Semigroup p) => Monoid (PatchMap k p) where
  mempty = PatchMap Map.empty

derive instance newtypePatchMap :: Newtype (PatchMap k p) _

_PatchMap :: forall k p. Iso' (PatchMap k p) (Map k p)
_PatchMap = _Newtype

instance functorPatchMap :: Functor (PatchMap k) where
  map f (PatchMap m) = PatchMap (map f m)

instance functorWithIndexPatchMap :: FunctorWithIndex k (PatchMap k) where
  mapWithIndex f (PatchMap m) = PatchMap (mapWithIndex f m)

instance foldlablePatchMap :: Foldable (PatchMap k) where
  foldr f z (PatchMap m) = foldr f z m
  foldl f z (PatchMap m) = foldl f z m
  foldMap f (PatchMap m) = foldMap f m

instance foldlableWithIndexPatchMap :: FoldableWithIndex k (PatchMap k) where
  foldrWithIndex f z (PatchMap m) = foldrWithIndex f z m
  foldlWithIndex f z (PatchMap m) = foldlWithIndex f z m
  foldMapWithIndex f (PatchMap m) = foldMapWithIndex f m

instance traversablePatchMap :: Traversable (PatchMap k) where
  traverse f (PatchMap m) = PatchMap <$> traverse f m
  sequence (PatchMap m) = PatchMap <$> sequence m

instance traversableWithIndexPatchMap :: TraversableWithIndex k (PatchMap k) where
  traverseWithIndex f (PatchMap m) = PatchMap <$> traverseWithIndex f m

applyPatchMap :: forall k p v. Ord k => (p -> v -> v) -> PatchMap k p -> Map k v -> Map k v
applyPatchMap applyPatchValue (PatchMap p) = mapWithIndex f
  where
    f k v =
      case Map.lookup k p of
        Nothing -> v
        Just pv -> applyPatchValue pv v

type NgramsTablePatch = PatchMap NgramsTerm NgramsPatch

type ReParent a = forall m. MonadState NgramsTable m => a -> m Unit

reParent :: Maybe NgramsTerm -> ReParent NgramsTerm
reParent parent child =
  at child <<< _Just <<< _NgramsElement <<< _parent .= parent

-- reParentNgramsPatch :: NgramsTerm -> ReParent NgramsPatch
-- ^ GHC would have accepted this type. Here reParentNgramsPatch checks but
--   not its usage in reParentNgramsTablePatch.
reParentNgramsPatch :: forall m. MonadState NgramsTable m
                    => NgramsTerm -> NgramsPatch -> m Unit
reParentNgramsPatch parent (NgramsPatch {patch_children: PatchSet {rem, add}}) = do
  traverse_ (reParent Nothing) rem
  traverse_ (reParent $ Just parent) add

reParentNgramsTablePatch :: ReParent NgramsTablePatch
reParentNgramsTablePatch = void <<< traverseWithIndex reParentNgramsPatch

applyNgramsTablePatch :: NgramsTablePatch -> NgramsTable -> NgramsTable
applyNgramsTablePatch p (NgramsTable m) =
  execState (reParentNgramsTablePatch p) $
  NgramsTable $ applyPatchMap applyNgramsPatch p m
    -- TODO: update the .root fields...
    -- See ROOT-UPDATE

type State =
  { ngramsTablePatch :: NgramsTablePatch
  , ngramsParent     :: Maybe NgramsTerm -- Nothing means we are not currently grouping terms
  , ngramsChildren   :: Map NgramsTerm Boolean
                     -- ^ Used only when grouping.
                     --   This updates the children of `ngramsParent`,
                     --   ngrams set to `true` are to be added, and `false` to
                     --   be removed.
  , searchQuery      :: String
  , termListFilter   :: Maybe TermList -- Nothing means all
  , termTypeFilter   :: Maybe TermType -- Nothing means all
  }

_ngramsChildren = prop (SProxy :: SProxy "ngramsChildren")

initialState :: forall props. props -> State
initialState _ =
  { ngramsTablePatch: mempty
  , ngramsParent:     Nothing
  , ngramsChildren:   mempty
  , searchQuery:      ""
  , termListFilter:   Nothing
  , termTypeFilter:   Nothing
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
  | SetTermListFilter (Maybe TermList)
  | SetTermTypeFilter (Maybe TermType)
  | SetSearchQuery String

data Mode = Authors | Sources | Institutes | Terms

derive instance genericMode :: Generic Mode _

instance showMode :: Show Mode where
  show = genericShow

derive instance eqMode :: Eq Mode

type Dispatch = Action -> Effect Unit

tableContainer :: { searchQuery :: String
                  , dispatch :: Dispatch
                  , ngramsParent :: Maybe NgramsTerm
                  , ngramsChildren :: Map NgramsTerm Boolean
                  , ngramsTable :: NgramsTable
                  }
               -> T.TableContainerProps -> Array ReactElement
tableContainer {searchQuery, dispatch, ngramsParent, ngramsChildren, ngramsTable: ngramsTableCache} props =
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
              [ div [className "savediv pull-left col-md-2", style { marginTop :"1.5em"}]
                [ span [className "needsaveicon glyphicon glyphicon-import"] []
                , button [_id "ImportListOrSaveAll", className "btn btn-warning", style {fontSize : "120%"}]
                  [ text "Import a Termlist" ]
                ]
              , div [className "col-md-4", style {marginTop : "37px"}]
                [ input [ className "form-control "
                        , _id "id_password"
                        , name "search", placeholder "Search"
                        , _type "value"
                        , value searchQuery
                        , onInput \e -> dispatch (SetSearchQuery (unsafeEventValue e))
                        ]
                ]
              , div [_id "filter_terms", className "col-md-6", style{ marginTop : "2.1em",paddingLeft :"1em"}]
                [ div [className "row", style {marginTop : "6px"}]
                  [ div [className "col-md-3"]
                    [ select  [ _id "picklistmenu"
                              , className "form-control custom-select"
                              , onChange (\e -> dispatch (SetTermListFilter $ readTermList $ unsafeEventValue e))
                              ] $ map optps1 termLists
                    ]
                  , div [className "col-md-3"]
                    [ select  [ _id "picktermtype"
                              , className "form-control custom-select"
                              , style {marginLeft : "1em"}
                              , onChange (\e -> dispatch (SetTermTypeFilter $ readTermType $ unsafeEventValue e))
                              ] $ map optps1 termTypes
                    ]
                  , div [className "col-md-3"] [ props.pageSizeControl ]
                  ]
                ]
              , div [className "col-md-6", style {marginTop : "24px", marginBottom : "14px"}]
                [ props.pageSizeDescription
                , props.paginationLinks
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
                ngramsClick child
                  | child == ngrams = Nothing
                  | otherwise       = Just $ dispatch $ ToggleChild false child
              in
              [ p[] [text $ "Editing " <> ngrams]
              , renderNgramsTree { ngramsTable, ngrams, ngramsStyle: [], ngramsClick }
              , button [className "btn btn-primary", onClick $ const $ dispatch $ AddTermChildren] [text "Save"]
              , button [className "btn btn-secondary", onClick $ const $ dispatch $ SetParentResetChildren Nothing] [text "Cancel"]
              ]) ngramsParent)
          , div [ _id "terms_table", className "panel-body" ]
                [ table [ className "table able table-bordered" ]
                  [ thead [ className "tableHeader table-bordered"] [props.tableHead]
                  , tbody [] props.tableBody
                  ]
                ]
          ]
        ]
      ]
    ]
  ]

commitPatch :: NgramsTablePatch -> StateCoTransformer State Unit
commitPatch pt = modifyState_ $ \s -> s { ngramsTablePatch = pt <> s.ngramsTablePatch }

toggleMap :: forall a. a -> Maybe a -> Maybe a
toggleMap _ (Just _) = Nothing
toggleMap b Nothing  = Just b

ngramsTableSpec' :: Spec State Props' Action
ngramsTableSpec' = simpleSpec performAction render
  where
    setParentResetChildren :: Maybe NgramsTerm -> State -> State
    setParentResetChildren p = _ { ngramsParent = p, ngramsChildren = mempty }

    performAction :: PerformAction State Props' Action
    performAction (SetTermListFilter c) _ _ = modifyState_ $ _ { termListFilter = c }
    performAction (SetTermTypeFilter c) _ _ = modifyState_ $ _ { termTypeFilter = c }
    performAction (SetSearchQuery s) _ _ = modifyState_ $ _ { searchQuery = s }
    performAction (SetParentResetChildren p) _ _ =
      modifyState_ $ setParentResetChildren p
    performAction (ToggleChild b c) _ _ =
      modifyState_ $ _ngramsChildren <<< at c %~ toggleMap b
    performAction (SetTermListItem n pl) _ _ = commitPatch pt
      where
        pe = NgramsPatch { patch_list: pl, patch_children: mempty }
        pt = PatchMap $ Map.singleton n pe
    performAction AddTermChildren _ {ngramsParent: Nothing} =
        -- impossible but harmless
        pure unit
    performAction AddTermChildren _
                  { ngramsParent: Just parent
                  , ngramsChildren
                  , ngramsTablePatch
                  } = do
        modifyState_ $ setParentResetChildren Nothing
        commitPatch pt
      where
        pc = patchSetFromMap ngramsChildren
        pe = NgramsPatch { patch_list: mempty, patch_children: pc }
        pt = PatchMap $ Map.fromFoldable [Tuple parent pe]
        -- TODO ROOT-UPDATE
        -- patch the root of the child to be equal to the root of the parent.

    render :: Render State Props' Action
    render dispatch { path: {nodeId, mode}
                    , loaded: initTable
                    , dispatch: loaderDispatch }
                    { ngramsTablePatch, ngramsParent, ngramsChildren, searchQuery }
                    _reactChildren =
      [ T.tableElt
          { rows
          , setParams: \params -> loaderDispatch (Loader.SetPath {nodeId, params, mode})
          , container: tableContainer {searchQuery, dispatch, ngramsParent, ngramsChildren, ngramsTable}
          , colNames:
              T.ColumnName <$>
              [ "Graph"
              , "Stop"
              , "Terms"
              , "Occurences (nb)"
              ]
          , totalRecords: 47361 -- TODO
          }
      ]
          where
            ngramsTable = applyNgramsTablePatch ngramsTablePatch initTable
            rows = convertRow <$> Map.toUnfoldable (Map.filter displayRow (ngramsTable ^. _NgramsTable))
            isRoot (NgramsElement e) = e.parent == Nothing
            -- TODO: There is a missing case where we display a row that we should not.
            -- Assumptions:
            --   * cats -> cat -> animal
            --   * We are editing cats: ngramsParent == Just "cats"
            --   * animal should not be listed since this would create a cycle!
            displayRow e@(NgramsElement {ngrams, children, parent}) =
              isRoot e
                -- ^ Display only nodes with parents
                      && (ngramsChildren ^. at ngrams /= Just true)
                      -- ^ and which are not scheduled to be added already.
                      && (case ngramsParent of
                            Just p ->
                              ngrams /= p &&
                              -- ^ and which is not the node being currently edited.
                              not (Set.member p children)
                              -- ^ ... or one of its children.
                            Nothing -> true)
              || -- Unless they are scheduled to be removed.
                 (ngramsChildren ^. at ngrams == Just false)
            convertRow (Tuple ngrams (NgramsElement { occurrences, list })) =
              { row:
                  renderNgramsItem { ngramsTable, ngrams, occurrences, ngramsParent, termList: list, dispatch }
              , delete: false
              }

initialPageParams :: Int -> Mode -> PageParams
initialPageParams nodeId mode = {nodeId, params: T.initialParams, mode}

type PageLoaderProps =
  { path :: PageParams
--, corpusInfo :: Maybe (NodePoly CorpusInfo)
  }

getTable :: TabType -> Maybe Int -> Aff NgramsTable
getTable tab = get <<< toUrl Back (Ngrams tab Nothing)

modeTabType :: Mode -> TabType
modeTabType Authors = TabAuthors
modeTabType Sources = TabSources
modeTabType Institutes = TabInstitutes
modeTabType Terms = TabTerms

loadPage :: PageParams -> Aff NgramsTable
loadPage {nodeId, mode} = getTable (modeTabType mode) (Just nodeId) -- TODO this ignores params

ngramsLoaderClass :: Loader.LoaderClass PageParams NgramsTable
ngramsLoaderClass = Loader.createLoaderClass "NgramsLoader" loadPage

ngramsLoader :: Loader.Props' PageParams NgramsTable -> ReactElement
ngramsLoader props = React.createElement ngramsLoaderClass props []

ngramsTableSpec :: Spec {} Props Void
ngramsTableSpec = simpleSpec defaultPerformAction render
  where
    render :: Render {} Props Void
    render _ {path: nodeId, mode} _ _ =
      -- TODO: ignored ignored loaded: corpusInfo
      [ ngramsLoader { path: initialPageParams nodeId mode
                     , component: createClass "NgramsTableLayout" ngramsTableSpec' initialState
                     } ]

tree :: { ngramsTable :: NgramsTable
        , ngramsStyle :: Array DOM.Props
        , ngramsClick :: NgramsTerm -> Maybe (Effect Unit)
        } -> NgramsTerm -> ReactElement
tree params@{ngramsTable, ngramsStyle, ngramsClick} label =
  li [ style {width : "100%"} ]
     [ i icon []
     , tag [text $ " " <> label]
     , forest cs
     ]
  where
    tag =
      case ngramsClick label of
        Just effect ->
          a (ngramsStyle <> [onClick $ const effect])
        Nothing ->
          span ngramsStyle
    leaf = List.null cs
    icon = gray <> [className $ "fas fa-caret-" <> if open then "down" else "right"]
    open = not leaf || false {- TODO -}
    gray = if leaf then [style {color: "#adb5bd"}] else []
    cs   = ngramsTable ^.. ix label <<< _NgramsElement <<< _children <<< folded

    forest = ul [] <<< map (tree params) <<< List.toUnfoldable

renderNgramsTree :: { ngrams      :: NgramsTerm
                    , ngramsTable :: NgramsTable
                    , ngramsStyle :: Array DOM.Props
                    , ngramsClick :: NgramsTerm -> Maybe (Effect Unit)
                    } -> ReactElement
renderNgramsTree { ngramsTable, ngrams, ngramsStyle, ngramsClick } =
  ul [] [
    span [className "tree"] [tree {ngramsTable, ngramsStyle, ngramsClick} ngrams]
  ]

renderNgramsItem :: { ngrams :: NgramsTerm
                    , ngramsTable :: NgramsTable
                    , occurrences :: Int
                    , termList :: TermList
                    , ngramsParent :: Maybe NgramsTerm
                    , dispatch :: Action -> Effect Unit
                    } -> Array ReactElement
renderNgramsItem { ngramsTable, ngrams, occurrences, termList, ngramsParent, dispatch } =
  [ checkbox GraphTerm
  , checkbox StopTerm
  , if ngramsParent == Nothing
    then renderNgramsTree { ngramsTable, ngrams, ngramsStyle, ngramsClick }
    else
      a [onClick $ const $ dispatch $ ToggleChild true ngrams]
        [ i [className "fas fa-plus"] []
        , span ngramsStyle [text $ " " <> ngrams]
        ]
  , text $ show occurrences
  ]
  where
    ngramsStyle = [termStyle termList]
    ngramsClick = Just <<< dispatch <<< SetParentResetChildren <<< Just
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
