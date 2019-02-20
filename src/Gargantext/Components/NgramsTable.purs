module Gargantext.Components.NgramsTable
  ( Props
  , PageParams
  , PatchMap
  , NgramsPatch
  , NgramsTable
  , VersionedNgramsTable
  , Version
  , Versioned(..)
  , Action
  , initialPageParams
  , initialState
  , ngramsTableSpec
  )
  where

import Control.Monad.State (class MonadState, execState)
import Control.Monad.Cont.Trans (lift)
import Data.Array (head)
import Data.Argonaut ( Json, class DecodeJson, decodeJson, class EncodeJson, encodeJson
                     , jsonEmptyObject, fromObject, (:=), (~>), (.?), (.??) )
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldlWithIndex, foldrWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Newtype (class Newtype)
import Data.Lens (Iso', Lens', (%~), (.=), (^.), (^..))
import Data.Lens.Common (_Just)
import Data.Lens.At (class At, at)
import Data.Lens.Index (class Index, ix)
import Data.Lens.Fold (folded)
import Data.Lens.Record (prop)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (class Traversable, traverse, traverse_, sequence)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Set (Set)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (error)
import Effect.Aff (Aff, throwError)
import Foreign.Object as FO
import React (ReactElement)
import React.DOM (a, button, div, h2, i, input, li, option, p, select, span, table, tbody, text, thead, ul)
import React.DOM.Props (_id, _type, checked, className, name, onChange, onClick, onInput, placeholder, style, value)
import React.DOM.Props as DOM
import Thermite (PerformAction, Render, Spec, StateCoTransformer, modifyState_, simpleSpec)
import Unsafe.Coerce (unsafeCoerce)

import Gargantext.Types (TermList(..), TermType, readTermList, readTermType, termLists, termTypes)
import Gargantext.Config (toUrl, End(..), Path(..), TabType)
import Gargantext.Config.REST (put)
import Gargantext.Components.AutoUpdate (autoUpdateElt)
import Gargantext.Components.Table as T
import Gargantext.Prelude
import Gargantext.Components.Loader as Loader

type Props a mode = Loader.InnerProps Int a ( mode :: mode )

type PageParams =
  { nodeId :: Int
  , listIds :: Array Int
  , params :: T.Params
  , tabType :: TabType
  }

initialPageParams :: Int -> Array Int -> TabType -> PageParams
initialPageParams nodeId listIds tabType = {nodeId, listIds, params: T.initialParams, tabType}

type Props' = Loader.InnerProps PageParams VersionedNgramsTable ()

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

type Version = Int

newtype Versioned a = Versioned
  { version :: Version
  , data    :: a
  }

instance encodeJsonVersioned :: EncodeJson a => EncodeJson (Versioned a) where
  encodeJson (Versioned {version, data: data_})
     = "version" := version
    ~> "data" := data_
    ~> jsonEmptyObject

instance decodeJsonVersioned :: DecodeJson a => DecodeJson (Versioned a) where
  decodeJson json = do
    obj     <- decodeJson json
    version <- obj .? "version"
    data_   <- obj .? "data"
    pure $ Versioned {version, data: data_}

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

type VersionedNgramsTable = Versioned NgramsTable

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

instance encodeJsonReplace :: EncodeJson a => EncodeJson (Replace a) where
  encodeJson Keep
    = "tag" := "Keep"
   ~> jsonEmptyObject
  encodeJson (Replace {old, new})
    = "old" := old
   ~> "new" := new
   ~> "tag" := "Replace"
   ~> jsonEmptyObject

instance decodeJsonReplace :: (DecodeJson a, Eq a) => DecodeJson (Replace a) where
  decodeJson json = do
    obj  <- decodeJson json
    mold <- obj .?? "old"
    mnew <- obj .?? "new"
    case Tuple mold mnew of
      Tuple (Just old) (Just new) -> pure $ replace old new
      Tuple Nothing Nothing       -> pure Keep
      _                           -> Left "decodeJsonReplace"

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

instance encodeJsonPatchSet :: EncodeJson a => EncodeJson (PatchSet a) where
  encodeJson (PatchSet {rem, add})
    -- TODO only include non empty fields
    = "rem" := (Set.toUnfoldable rem :: Array a)
   ~> "add" := (Set.toUnfoldable add :: Array a)
   ~> jsonEmptyObject

instance decodeJsonPatchSet :: (Ord a, DecodeJson a) => DecodeJson (PatchSet a) where
  decodeJson json = do
    -- TODO handle empty fields
    obj <- decodeJson json
    rem <- mkSet <$> (obj .? "rem")
    add <- mkSet <$> (obj .? "add")
    pure $ PatchSet { rem, add }
   where
    mkSet :: forall b. Ord b => Array b -> Set b
    mkSet = Set.fromFoldable

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

instance encodeJsonNgramsPatch :: EncodeJson NgramsPatch where
  -- TODO only include non empty fields
  encodeJson (NgramsPatch { patch_children, patch_list })
     = "patch_children" := patch_children
    ~> "patch_list"     := patch_list
    ~> jsonEmptyObject

instance decodeJsonNgramsPatch :: DecodeJson NgramsPatch where
  decodeJson json = do
    obj            <- decodeJson json
    -- TODO handle empty fields
    patch_list     <- obj .? "patch_list"
    patch_children <- obj .? "patch_children"
    pure $ NgramsPatch { patch_list, patch_children }

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

instance encodeJsonPatchMap :: EncodeJson p => EncodeJson (PatchMap String p) where
  encodeJson (PatchMap m) =
    encodeJson $ FO.fromFoldable $ (Map.toUnfoldable m :: Array _)

instance decodeJsonPatchMap :: DecodeJson p => DecodeJson (PatchMap String p) where
  decodeJson json = do
    obj <- decodeJson json
    pure $ PatchMap $ Map.fromFoldableWithIndex (obj :: FO.Object p)

isEmptyPatchMap :: forall k p. PatchMap k p -> Boolean
isEmptyPatchMap (PatchMap p) = Map.isEmpty p

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
  , ngramsVersion    :: Version
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

initialState :: forall props. { loaded :: VersionedNgramsTable | props }
             -> State
initialState {loaded: Versioned {version}} =
  { ngramsTablePatch: mempty
  , ngramsVersion:    version
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
  | Refresh

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
              [
              {-div [className "savediv pull-left col-md-2", style { marginTop :"35px"}]
                [  button [_id "ImportListOrSaveAll", className "btn btn-warning", style {fontSize : "120%"}]
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
              ,-}
                div [_id "filter_terms", className "col-md-6", style{ marginTop : "2.1em",paddingLeft :"1em"}]
                [ div [className "col-md-10 list-group", style {marginTop : "6px"}]
                  [ li [className " list-group-item"]
                    [ select  [ _id "picklistmenu"
                              , className "form-control custom-select"
                              , onChange (\e -> dispatch (SetTermListFilter $ readTermList $ unsafeEventValue e))
                              ] $ map optps1 termLists
                    ]
                  , li [className "list-group-item"]
                    [ select  [ _id "picktermtype"
                              , className "form-control custom-select"
                              , onChange (\e -> dispatch (SetTermTypeFilter $ readTermType $ unsafeEventValue e))
                              ] $ map optps1 termTypes
                    ]
                  , li [className " list-group-item"] [ props.pageSizeControl ]
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

putTable :: {nodeId :: Int, listIds :: Array Int, tabType :: TabType} -> Versioned NgramsTablePatch -> Aff (Versioned NgramsTablePatch)
putTable {nodeId, listIds, tabType} =
  put (toUrl Back (PutNgrams tabType (head listIds)) $ Just nodeId)

commitPatch :: {nodeId :: Int, listIds :: Array Int, tabType :: TabType}
            -> Versioned NgramsTablePatch -> StateCoTransformer State Unit
commitPatch props pt@(Versioned {data: tablePatch}) = do
  Versioned {version: newVersion, data: newPatch} <- lift $ putTable props pt
  modifyState_ $ \s ->
    s { ngramsVersion    = newVersion
      , ngramsTablePatch = newPatch <> tablePatch <> s.ngramsTablePatch
      }
    -- TODO: check that pt.version == s.ngramsTablePatch.version

toggleMap :: forall a. a -> Maybe a -> Maybe a
toggleMap _ (Just _) = Nothing
toggleMap b Nothing  = Just b

ngramsTableSpec :: Spec State Props' Action
ngramsTableSpec = simpleSpec performAction render
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

    render :: Render State Props' Action
    render dispatch { path: {nodeId, listIds, tabType}
                    , loaded: Versioned { data: initTable }
                    , dispatch: loaderDispatch }
                    { ngramsTablePatch, ngramsParent, ngramsChildren, searchQuery }
                    _reactChildren =
      [ autoUpdateElt { duration: 3000
                      , effect:   dispatch Refresh
                      }
      , T.tableElt
          { rows
          , setParams: \params -> loaderDispatch (Loader.SetPath {nodeId, listIds, params, tabType})
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
