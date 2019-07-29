module Gargantext.Components.NgramsTable.Core
  ( PageParams
  , CoreParams
  , NgramsElement(..)
  , _NgramsElement
  , NgramsPatch(..)
  , NgramsTable(..)
  , NgramsTablePatch
  , NewElems
  , NgramsPatches
  , _NgramsTable
  , NgramsTerm
  , normNgram
  , findNgramTermList
  , Version
  , Versioned(..)
  , VersionedNgramsTable
  , CoreState
  , LoadedNgramsTableProps
  , highlightNgrams
  , initialPageParams
  , loadNgramsTable
  , ngramsLoader
  , ngramsLoaderClass
  , convOrderBy
  , Replace(..) -- Ideally we should keep the constructors hidden
  , replace
  , PatchSet(..)
  , PatchMap(..)
  , patchSetFromMap
  , applyPatchSet
  , applyNgramsTablePatch
  , singletonPatchMap
  , fromNgramsPatches
  , singletonNgramsTablePatch
  , _list
  , _occurrences
  , _children
  , _ngrams
  , _parent
  , _root
  , commitPatch
  , addNewNgram
  )
  where

import Control.Monad.State (class MonadState, execState)
import Control.Monad.Cont.Trans (lift)
import Data.Array (head)
import Data.Array as A
import Data.Argonaut ( class DecodeJson, decodeJson, class EncodeJson, encodeJson
                     , jsonEmptyObject, (:=), (~>), (.?), (.??) )
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldlWithIndex, foldrWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Newtype (class Newtype)
import Data.Lens (Iso', Lens', use, view, (%=), (.~), (?=), (^.), (^?))
import Data.Lens.Common (_Just)
import Data.Lens.At (class At, at)
import Data.Lens.Index (class Index, ix)
import Data.Lens.Fold (folded, traverseOf_)
import Data.Lens.Record (prop)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.List ((:), List(Nil))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (class Traversable, traverse, traverse_, sequence)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Set (Set)
import Data.Set as Set
import Data.String as S
import Data.String.Regex as R
import Data.String.Regex.Flags as R
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
-- import Debug.Trace
import Effect.Aff (Aff)
import Foreign.Object as FO
import React (ReactElement)
import React as React
import Thermite (StateCoTransformer, modifyState_)
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)

import Gargantext.Utils.KarpRabin (indicesOfAny)
import Gargantext.Types (TermList(..), TermSize)
import Gargantext.Config (toUrl, endConfigStateful, End(..), Path(..), TabType, OrderBy(..), CTabNgramType(..))
import Gargantext.Config.REST (get, put, post)
import Gargantext.Components.Table as T
import Gargantext.Prelude
import Gargantext.Components.Loader as Loader

type CoreParams s =
  { nodeId  :: Int
    -- ^ This node can be a corpus or contact.
  , listIds :: Array Int
  , tabType :: TabType
  | s
  }

type PageParams =
  CoreParams
    ( params :: T.Params
    , searchQuery :: String
    , termListFilter :: Maybe TermList -- Nothing means all
    , termSizeFilter :: Maybe TermSize -- Nothing means all
    )

initialPageParams :: Int -> Array Int -> TabType -> PageParams
initialPageParams nodeId listIds tabType =
  { nodeId
  , listIds
  , params: T.initialParams
  , tabType
  , termSizeFilter: Nothing
  , termListFilter: Just GraphTerm
  , searchQuery: ""
  }

type NgramsTerm = String

-----------------------------------------------------------------------------------
newtype NgramsElement = NgramsElement
  { ngrams      :: NgramsTerm
  , list        :: TermList
  , occurrences :: Int
  , parent      :: Maybe NgramsTerm
  , root        :: Maybe NgramsTerm
  , children    :: Set NgramsTerm
  }

derive instance eqNgramsElement :: Eq NgramsElement
derive instance eqNgramsTable :: Eq NgramsTable


_parent = prop (SProxy :: SProxy "parent")
_root   = prop (SProxy :: SProxy "root")
_ngrams = prop (SProxy :: SProxy "ngrams")

_children :: forall row. Lens' { children :: Set NgramsTerm | row } (Set NgramsTerm)
_children = prop (SProxy :: SProxy "children")

_occurrences :: forall row. Lens' { occurrences :: Int | row } Int
_occurrences = prop (SProxy :: SProxy "occurrences")

_list :: forall a row. Lens' { list :: a | row } a
_list = prop (SProxy :: SProxy "list")

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
    root        <- obj .?? "root"
    children'   <- obj .?  "children"
    let children = Set.fromFoldable (children' :: Array NgramsTerm)
    pure $ NgramsElement {ngrams, list, occurrences, parent, root, children}

-----------------------------------------------------------------------------------
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
-----------------------------------------------------------------------------------

wordBoundaryChars :: String
wordBoundaryChars = "[ .,;:!?'\\{}()]"

wordBoundaryReg = case R.regex ("(" <> wordBoundaryChars <> ")") (R.global <> R.multiline) of
  Left e  -> unsafePartial $ crashWith e
  Right r -> r

wordBoundaryReg2 = case R.regex ("(" <> wordBoundaryChars <> ")\\1") (R.global <> R.multiline) of
  Left e  -> unsafePartial $ crashWith e
  Right r -> r

-- TODO: while this function works well with word boundaries,
--       it inserts too many spaces.
highlightNgrams :: CTabNgramType -> NgramsTable -> String -> Array (Tuple String (Maybe TermList))
highlightNgrams ntype (NgramsTable table) input0 =
    -- trace {pats, input0, input, ixs} \_ ->
    let sN = unsafePartial (foldl goFold {i0: 0, s: input, l: Nil} ixs) in
    A.reverse (A.fromFoldable (consNonEmpty (undb (init sN.s)) sN.l))
  where
    spR x = " " <> R.replace wordBoundaryReg "$1$1" x <> " "
    reR = R.replace wordBoundaryReg " "
    db = S.replace (S.Pattern " ") (S.Replacement "  ")
    sp x = " " <> db x <> " "
    undb = R.replace wordBoundaryReg2 "$1"
    init x = S.take (S.length x - 1) x
    input = spR input0
    pats = A.fromFoldable (Map.keys table)
    ixs = indicesOfAny (sp <$> pats) (normNgram ntype input)

    consOnJustTail s xs@(Tuple _ (Just _) : _) =
      Tuple s Nothing : xs
    consOnJustTail _ xs = xs

    consNonEmpty x xs
      | S.null x  = xs
      | otherwise = Tuple x Nothing : xs

    -- NOTE that only the first matching pattern is used, the others are ignored!
    goFold :: Partial => _ -> Tuple Int (Array Int) -> _
    goFold { i0, s, l } (Tuple i pis)
      | i < i0    =
        -- Skip this pattern which is overlapping with a previous one.
        { i0, s, l }
      | otherwise =
      case A.index pis 0 of
        Nothing ->
          { i0, s, l }
        Just pi ->
          case A.index pats pi of
            Nothing ->
              crashWith "highlightNgrams: out of bounds pattern"
            Just pat ->
              let lpat = S.length (db pat) in
              case Map.lookup pat table of
                Nothing ->
                  crashWith "highlightNgrams: pattern missing from table"
                Just (NgramsElement ne) ->
                  let
                    s1    = S.splitAt (i - i0) s
                    s2    = S.splitAt lpat     (S.drop 1 s1.after)
                    s3    = S.splitAt 1        s2.after
                    unspB = if i0 == 0 then S.drop 1 else identity
                    s3b   = s3.before
                  in
                  -- trace {s, i, i0, s1, s2, s3, pat, lpat, s3b} \_ ->
                  -- `undb s2.before` and pat might differ by casing only!
                  { i0: i + lpat + 2
                  , s:  s3.after
                  , l:  Tuple (undb s2.before) (Just ne.list) :
                        consOnJustTail s3b
                        (consNonEmpty (unspB (undb s1.before)) l)
                  }

-----------------------------------------------------------------------------------

type VersionedNgramsTable = Versioned NgramsTable

-----------------------------------------------------------------------------------
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
  , occurrences: e.occurrences
  , parent:      e.parent
  , root:        e.root
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

singletonPatchMap :: forall k p. k -> p -> PatchMap k p
singletonPatchMap k p = PatchMap (Map.singleton k p)

isEmptyPatchMap :: forall k p. PatchMap k p -> Boolean
isEmptyPatchMap (PatchMap p) = Map.isEmpty p

applyPatchMap :: forall k p v. Ord k => (p -> v -> v) -> PatchMap k p -> Map k v -> Map k v
applyPatchMap applyPatchValue (PatchMap p) = mapWithIndex f
  where
    f k v =
      case Map.lookup k p of
        Nothing -> v
        Just pv -> applyPatchValue pv v

type NgramsPatches = PatchMap NgramsTerm NgramsPatch

type NewElems = Map NgramsTerm TermList

type NgramsTablePatch =
  { ngramsNewElems :: NewElems
  , ngramsPatches  :: NgramsPatches
  }

fromNgramsPatches :: NgramsPatches -> NgramsTablePatch
fromNgramsPatches ngramsPatches = {ngramsNewElems: mempty, ngramsPatches}

normNgram :: CTabNgramType -> String -> NgramsTerm
normNgram CTabAuthors = identity
normNgram CTabSources = identity
normNgram CTabInstitutes = identity
normNgram CTabTerms      = S.toLower <<< R.replace wordBoundaryReg " "


findNgramTermList :: CTabNgramType -> NgramsTable -> String -> Maybe TermList
findNgramTermList ntype (NgramsTable m) s = m ^? at (normNgram ntype s) <<< _Just <<< _NgramsElement <<< _list

singletonNgramsTablePatch :: CTabNgramType -> NgramsTerm -> NgramsPatch -> NgramsTablePatch
singletonNgramsTablePatch m n p = fromNgramsPatches $ singletonPatchMap (normNgram m n) p

type RootParent = { root :: NgramsTerm, parent :: NgramsTerm }

type ReParent a = forall m. MonadState NgramsTable m => a -> m Unit

reRootChildren :: NgramsTerm -> ReParent NgramsTerm
reRootChildren root ngram = do
  nre <- use (at ngram)
  traverseOf_ (_Just <<< _NgramsElement <<< _children <<< folded) (\child -> do
    at child <<< _Just <<< _NgramsElement <<< _root ?= root
    reRootChildren root child) nre

reParent :: Maybe RootParent -> ReParent NgramsTerm
reParent mrp child = do
  at child <<< _Just <<< _NgramsElement %= ((_parent .~ (view _parent <$> mrp)) <<<
                                            (_root   .~ (view _root   <$> mrp)))
  reRootChildren (maybe child identity (mrp ^? _Just <<< _root)) child

-- reParentNgramsPatch :: NgramsTerm -> ReParent NgramsPatch
-- ^ GHC would have accepted this type. Here reParentNgramsPatch checks but
--   not its usage in reParentNgramsTablePatch.
reParentNgramsPatch :: forall m. MonadState NgramsTable m
                    => NgramsTerm -> NgramsPatch -> m Unit
reParentNgramsPatch parent (NgramsPatch {patch_children: PatchSet {rem, add}}) = do
  -- root_of_parent <- use (at parent <<< _Just <<< _NgramsElement <<< _root)
  -- ^ TODO this does not type checks, we do the following two lines instead:
  s <- use (at parent)
  let root_of_parent = s ^. (_Just <<< _NgramsElement <<< _root)
  let rp = { root: maybe parent identity root_of_parent, parent }
  traverse_ (reParent Nothing) rem
  traverse_ (reParent $ Just rp) add

reParentNgramsTablePatch :: ReParent NgramsPatches
reParentNgramsTablePatch = void <<< traverseWithIndex reParentNgramsPatch

newElemsTable :: NewElems -> Map NgramsTerm NgramsElement
newElemsTable = mapWithIndex newElem
  where
    newElem ngrams list =
      NgramsElement
        { ngrams
        , list
        , occurrences: 1
        , parent:      Nothing
        , root:        Nothing
        , children:    mempty
        }

applyNgramsTablePatch :: NgramsTablePatch -> NgramsTable -> NgramsTable
applyNgramsTablePatch { ngramsPatches, ngramsNewElems: n } (NgramsTable m) =
  execState (reParentNgramsTablePatch ngramsPatches) $
  NgramsTable $ applyPatchMap applyNgramsPatch ngramsPatches (newElemsTable n <> m)

-----------------------------------------------------------------------------------

type CoreState s =
  { ngramsTablePatch :: NgramsTablePatch
  , ngramsVersion    :: Version
  | s
  }

postNewNgrams :: forall s. Array NgramsTerm -> Maybe TermList -> CoreParams s -> Aff Unit
postNewNgrams newNgrams mayList {nodeId, listIds, tabType} =
  when (not (A.null newNgrams)) $ do
    (_ :: Array Unit) <- post (toUrl endConfigStateful Back (PutNgrams tabType (head listIds) mayList) $ Just nodeId) newNgrams
    pure unit

postNewElems :: forall s. NewElems -> CoreParams s -> Aff Unit
postNewElems newElems params = void $ traverseWithIndex postNewElem newElems
  where
    postNewElem ngrams list = postNewNgrams [ngrams] (Just list) params

addNewNgram :: CTabNgramType -> NgramsTerm -> TermList -> NgramsTablePatch
addNewNgram ntype ngrams list = { ngramsPatches: mempty
                          , ngramsNewElems: Map.singleton (normNgram ntype ngrams) list }

putNgramsPatches :: {nodeId :: Int, listIds :: Array Int, tabType :: TabType} -> Versioned NgramsPatches -> Aff (Versioned NgramsPatches)
putNgramsPatches {nodeId, listIds, tabType} =
  put (toUrl endConfigStateful Back (PutNgrams tabType (head listIds) Nothing) $ Just nodeId)

commitPatch :: forall s. {nodeId :: Int, listIds :: Array Int, tabType :: TabType}
            -> Versioned NgramsTablePatch -> StateCoTransformer (CoreState s) Unit
commitPatch props (Versioned {version, data: tablePatch@{ngramsPatches, ngramsNewElems}}) = do
  let pt = Versioned { version, data: ngramsPatches }
  lift $ postNewElems ngramsNewElems props
  Versioned {version: newVersion, data: newPatch} <- lift $ putNgramsPatches props pt
  modifyState_ $ \s ->
    s { ngramsVersion    = newVersion
      , ngramsTablePatch = fromNgramsPatches newPatch <> tablePatch <> s.ngramsTablePatch
      }
    -- TODO: check that pt.version == s.ngramsTablePatch.version

loadNgramsTable :: PageParams -> Aff VersionedNgramsTable
loadNgramsTable { nodeId, listIds, termListFilter, termSizeFilter
         , searchQuery, tabType, params: {offset, limit, orderBy}} =
  get $ toUrl endConfigStateful Back
          (GetNgrams { tabType, offset, limit, listIds
                     , orderBy: convOrderBy <$> orderBy
                     , termListFilter, termSizeFilter
                     , searchQuery
                     })
          (Just nodeId)

convOrderBy (T.ASC  (T.ColumnName "Score (Occurrences)")) = ScoreAsc
convOrderBy (T.DESC (T.ColumnName "Score (Occurrences)")) = ScoreDesc
convOrderBy (T.ASC  _) = TermAsc
convOrderBy (T.DESC _) = TermDesc

ngramsLoaderClass :: Loader.LoaderClass PageParams VersionedNgramsTable
ngramsLoaderClass = Loader.createLoaderClass "NgramsTableLoader" loadNgramsTable

ngramsLoader :: Loader.Props' PageParams VersionedNgramsTable -> ReactElement
ngramsLoader props = React.createElement ngramsLoaderClass props []

type LoadedNgramsTableProps = Loader.InnerProps PageParams VersionedNgramsTable ()
