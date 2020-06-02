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
  , ngramsTermText
  , findNgramTermList
  , Version
  , Versioned(..)
  , VersionedNgramsPatches
  , VersionedNgramsTable
  , CoreState
  , highlightNgrams
  , initialPageParams
  , loadNgramsTable
  , loadNgramsTableAll
  , convOrderBy
  , Replace(..) -- Ideally we should keep the constructors hidden
  , replace
  , PatchSet(..)
  , PatchMap(..)
  , _PatchMap
  , patchSetFromMap
  , applyPatchSet
  , applyNgramsTablePatch
  , applyNgramsPatches
  , rootsOf
  , singletonPatchMap
  , fromNgramsPatches
  , singletonNgramsTablePatch
  , isEmptyNgramsTablePatch
  , _list
  , _occurrences
  , _children
  , _ngrams
  , _parent
  , _root
  , commitPatch
  , commitPatchR
  , putNgramsPatches
  , syncPatches
  , syncPatchesR
  , addNewNgram
  , Action(..)
  , Dispatch
  , isSingleNgramsTerm
  , filterTermSize
  )
  where

import Prelude

import Control.Monad.Cont.Trans (lift)
import Control.Monad.State (class MonadState, execState)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonEmptyObject, (.:), (.:!), (:=), (~>))
import Data.Array (head)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldlWithIndex, foldrWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Lens (Iso', Lens', use, view, (%=), (.~), (?=), (^?))
import Data.Lens.At (class At, at)
import Data.Lens.Common (_Just)
import Data.Lens.Fold (folded, traverseOf_)
import Data.Lens.Index (class Index, ix)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List ((:), List(Nil))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.String as S
import Data.String.Common as DSC
import Data.String.Regex (Regex, regex, replace) as R
import Data.String.Regex.Flags (global, multiline) as R
import Data.String.Utils as SU
import Data.Symbol (SProxy(..))
import Data.Traversable (class Traversable, for, sequence, traverse, traverse_)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, launchAff_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception.Unsafe (unsafeThrow)
import Foreign.Object as FO
import Reactix (State) as R
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)
import Thermite (StateCoTransformer, modifyState_)

import Gargantext.Components.Table as T
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, get, put, post)
import Gargantext.Types (CTabNgramType(..), OrderBy(..), ScoreType(..), TabSubType(..), TabType(..), TermList(..), TermSize(..))
import Gargantext.Utils.KarpRabin (indicesOfAny)

type CoreParams s =
  { nodeId  :: Int
    -- ^ This node can be a corpus or contact.
  , listIds :: Array Int
  , tabType :: TabType
  , session :: Session
  | s
  }

type PageParams =
  CoreParams
    ( params         :: T.Params
    , searchQuery    :: String
    , termListFilter :: Maybe TermList -- Nothing means all
    , termSizeFilter :: Maybe TermSize -- Nothing means all
    , scoreType      :: ScoreType
    )

initialPageParams :: Session -> Int -> Array Int -> TabType -> PageParams
initialPageParams session nodeId listIds tabType =
  { nodeId
  , listIds
  , params
  , tabType
  , termSizeFilter: Nothing
  , termListFilter: Just GraphTerm
  , searchQuery: ""
  , scoreType: Occurrences
  , session
  }
  where
    params = T.initialParams { orderBy = Just (T.DESC $ T.ColumnName "Score") }

newtype NgramsTerm = NormNgramsTerm String

derive instance eqNgramsTerm  :: Eq  NgramsTerm
derive instance ordNgramsTerm :: Ord NgramsTerm

instance encodeJsonNgramsTerm :: EncodeJson NgramsTerm where
  encodeJson (NormNgramsTerm s) = encodeJson s

-- TODO we assume that the ngrams are already normalized.
instance decodeJsonNgramsTerm :: DecodeJson NgramsTerm where
  decodeJson = map NormNgramsTerm <<< decodeJson

ngramsTermText :: NgramsTerm -> String
ngramsTermText (NormNgramsTerm t) = t

-- TODO
normNgramInternal :: CTabNgramType -> String -> String
normNgramInternal CTabAuthors    = identity
normNgramInternal CTabSources    = identity
normNgramInternal CTabInstitutes = identity
normNgramInternal CTabTerms      = S.toLower <<< R.replace wordBoundaryReg " "

normNgram :: CTabNgramType -> String -> NgramsTerm
normNgram tabType = NormNgramsTerm <<< normNgramInternal tabType

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

_NgramsElement  :: Iso' NgramsElement {
    children    :: Set NgramsTerm
  , list        :: TermList
  , ngrams      :: NgramsTerm
  , occurrences :: Int
  , parent      :: Maybe NgramsTerm
  , root        :: Maybe NgramsTerm
  }
_NgramsElement = _Newtype

instance decodeJsonNgramsElement :: DecodeJson NgramsElement where
  decodeJson json = do
    obj         <- decodeJson json
    ngrams      <- obj .:  "ngrams"
    list        <- obj .:  "list"
    occurrences <- obj .:  "occurrences"
    parent      <- obj .:! "parent"
    root        <- obj .:! "root"
    children'   <- obj .:  "children"
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
    version <- obj .: "version"
    data_   <- obj .: "data"
    pure $ Versioned {version, data: data_}

-- type NgramsTable = Array (NTree NgramsElement)
-- type NgramsTable = Array NgramsElement
newtype NgramsTable = NgramsTable (Map NgramsTerm NgramsElement)

derive instance newtypeNgramsTable :: Newtype NgramsTable _

_NgramsTable :: Iso' NgramsTable (Map NgramsTerm NgramsElement)
_NgramsTable = _Newtype

instance indexNgramsTable :: Index NgramsTable NgramsTerm NgramsElement where
  ix k = _NgramsTable <<< ix k

instance atNgramsTable :: At NgramsTable NgramsTerm NgramsElement where
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

wordBoundaryReg :: R.Regex
wordBoundaryReg = case R.regex ("(" <> wordBoundaryChars <> ")") (R.global <> R.multiline) of
  Left e  -> unsafePartial $ crashWith e
  Right r -> r

wordBoundaryReg2 :: R.Regex
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
    ixs = indicesOfAny (sp <<< ngramsTermText <$> pats) (normNgramInternal ntype input)

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
              let lpat = S.length (db (ngramsTermText pat)) in
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

derive instance eqReplace :: Eq a => Eq (Replace a)

instance semigroupReplace :: Eq a => Semigroup (Replace a) where
  append Keep p = p
  append p Keep = p
  append (Replace { old }) (Replace { new }) | old /= new = unsafeThrow "old != new"
  append (Replace { new }) (Replace { old }) = replace old new

instance semigroupMonoid :: Eq a => Monoid (Replace a) where
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
    mold <- obj .:! "old"
    mnew <- obj .:! "new"
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
    rem <- mkSet <$> (obj .: "rem")
    add <- mkSet <$> (obj .: "add")
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

derive instance eqNgramsPatch  :: Eq NgramsPatch
derive instance eqPatchSetNgramsTerm  :: Eq (PatchSet NgramsTerm)

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
    patch_list     <- obj .: "patch_list"
    patch_children <- obj .: "patch_children"
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

instance semigroupPatchMap :: (Ord k, Eq p, Monoid p) => Semigroup (PatchMap k p) where
  append (PatchMap p) (PatchMap q) = PatchMap pMap
    where
      pMap = Map.filter (\v -> v /= mempty) $ Map.unionWith append p q

instance monoidPatchMap :: (Ord k, Eq p, Monoid p) => Monoid (PatchMap k p) where
  mempty = PatchMap Map.empty

derive instance newtypePatchMap :: Newtype (PatchMap k p) _
derive instance eqPatchMap :: (Eq k, Eq p) => Eq (PatchMap k p)

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

-- TODO generalize
instance encodeJsonPatchMap :: EncodeJson p => EncodeJson (PatchMap NgramsTerm p) where
  encodeJson (PatchMap m) =
    encodeJson $ FO.fromFoldable $ map (lmap ngramsTermText) (Map.toUnfoldable m :: Array _)

instance decodeJsonPatchMap :: DecodeJson p => DecodeJson (PatchMap NgramsTerm p) where
  decodeJson json = do
    obj <- decodeJson json
    pure $ PatchMap $ foldlWithIndex (\k m v -> Map.insert (NormNgramsTerm k) v m) mempty (obj :: FO.Object p)
    -- TODO we assume that the ngrams are already normalized ^^^^^^^^^^^^^

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
type VersionedNgramsPatches = Versioned NgramsPatches

type NewElems = Map NgramsTerm TermList

type NgramsTablePatch =
  { ngramsNewElems :: NewElems
  , ngramsPatches  :: NgramsPatches
  }

isEmptyNgramsTablePatch :: NgramsTablePatch -> Boolean
isEmptyNgramsTablePatch {ngramsPatches} = isEmptyPatchMap ngramsPatches

fromNgramsPatches :: NgramsPatches -> NgramsTablePatch
fromNgramsPatches ngramsPatches = {ngramsNewElems: mempty, ngramsPatches}

findNgramTermList :: NgramsTable -> NgramsTerm -> Maybe TermList
findNgramTermList (NgramsTable m) n = m ^? at n <<< _Just <<< _NgramsElement <<< _list

singletonNgramsTablePatch :: NgramsTerm -> NgramsPatch -> NgramsTablePatch
singletonNgramsTablePatch n p = fromNgramsPatches $ singletonPatchMap n p

rootsOf :: NgramsTable -> Set NgramsTerm
rootsOf (NgramsTable m) = Map.keys $ Map.mapMaybe isRoot m
  where
    isRoot (NgramsElement { parent }) = parent
-- rootsOf (NgramsTable m) = Map.keys $ Map.filter isRoot m
--   where
--     isRoot (NgramsElement {parent}) = isNothing parent

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
  reRootChildren (fromMaybe child (mrp ^? _Just <<< _root)) child

-- reParentNgramsPatch :: NgramsTerm -> ReParent NgramsPatch
-- ^ GHC would have accepted this type. Here reParentNgramsPatch checks but
--   not its usage in reParentNgramsTablePatch.
reParentNgramsPatch :: forall m. MonadState NgramsTable m
                    => NgramsTerm -> NgramsPatch -> m Unit
reParentNgramsPatch parent (NgramsPatch {patch_children: PatchSet {rem, add}}) = do
  -- root_of_parent <- use (at parent <<< _Just <<< _NgramsElement <<< _root)
  -- ^ TODO this does not type checks, we do the following two lines instead:
  s <- use (at parent)
  let root_of_parent = s ^? (_Just <<< _NgramsElement <<< _root <<< _Just)
  let rp = { root: fromMaybe parent root_of_parent, parent }
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

applyNgramsPatches :: forall s. CoreState s -> NgramsTable -> NgramsTable
applyNgramsPatches {ngramsLocalPatch, ngramsStagePatch, ngramsValidPatch} =
  applyNgramsTablePatch (ngramsLocalPatch <> ngramsStagePatch <> ngramsValidPatch)
-----------------------------------------------------------------------------------

type CoreState s =
  { ngramsLocalPatch :: NgramsTablePatch
                     -- ^ These patches are local and not yet staged.
  , ngramsStagePatch :: NgramsTablePatch
                     -- ^ These patches are staged (scheduled for synchronization).
                     --   Requests are being performed at the moment.
  , ngramsValidPatch :: NgramsTablePatch
                     -- ^ These patches have been synchronized with the server.
  , ngramsVersion    :: Version
  | s
  }

postNewNgrams :: forall s. Array NgramsTerm -> Maybe TermList -> CoreParams s -> Aff Unit
postNewNgrams newNgrams mayList {nodeId, listIds, tabType, session} =
  when (not (A.null newNgrams)) $ do
    (_ :: Array Unit) <- post session p newNgrams
    pure unit
  where p = PutNgrams tabType (head listIds) mayList (Just nodeId)

postNewElems :: forall s. NewElems -> CoreParams s -> Aff Unit
postNewElems newElems params = void $ traverseWithIndex postNewElem newElems
  where
    postNewElem ngrams list = postNewNgrams [ngrams] (Just list) params

addNewNgram :: NgramsTerm -> TermList -> NgramsTablePatch
addNewNgram ngrams list =
  { ngramsPatches: mempty
  , ngramsNewElems: Map.singleton ngrams list }

putNgramsPatches :: forall s. CoreParams s -> VersionedNgramsPatches -> Aff VersionedNgramsPatches
putNgramsPatches {session, nodeId, listIds, tabType} = put session putNgrams
  where putNgrams = PutNgrams tabType (head listIds) Nothing (Just nodeId)

-- DEPRECATED: use the Reactix version `syncPatchesR`
syncPatches :: forall p s. CoreParams p -> CoreState s -> StateCoTransformer (CoreState s) Unit
syncPatches props { ngramsLocalPatch: ngramsLocalPatch@{ngramsNewElems, ngramsPatches}
                  , ngramsStagePatch
                  , ngramsValidPatch
                  , ngramsVersion
                  } = do
  when (isEmptyNgramsTablePatch ngramsStagePatch) $ do
    modifyState_ $ \s ->
      s { ngramsLocalPatch = fromNgramsPatches mempty
        , ngramsStagePatch = ngramsLocalPatch
        }
    let pt = Versioned { version: ngramsVersion, data: ngramsPatches }
    lift $ postNewElems ngramsNewElems props
    Versioned {version: newVersion, data: newPatch} <- lift $ putNgramsPatches props pt
    modifyState_ $ \s ->
      s { ngramsVersion    = newVersion
        , ngramsValidPatch = fromNgramsPatches newPatch <> ngramsLocalPatch <> s.ngramsValidPatch
        , ngramsStagePatch = fromNgramsPatches mempty
        }

syncPatchesR :: forall p s. CoreParams p -> R.State (CoreState s) -> Effect Unit
syncPatchesR props ({ ngramsLocalPatch: ngramsLocalPatch@{ngramsNewElems, ngramsPatches}
                   , ngramsStagePatch
                   , ngramsValidPatch
                   , ngramsVersion
                   } /\ setState) = do
  when (isEmptyNgramsTablePatch ngramsStagePatch) $ do
    setState $ \s ->
      s { ngramsLocalPatch = fromNgramsPatches mempty
        , ngramsStagePatch = ngramsLocalPatch
        }
    let pt = Versioned { version: ngramsVersion, data: ngramsPatches }
    launchAff_ $ do
      _ <- postNewElems ngramsNewElems props
      Versioned {version: newVersion, data: newPatch} <- putNgramsPatches props pt
      liftEffect $ setState $ \s ->
        s { ngramsVersion    = newVersion
          , ngramsValidPatch = fromNgramsPatches newPatch <> ngramsLocalPatch <> s.ngramsValidPatch
          , ngramsStagePatch = fromNgramsPatches mempty
          }

-- DEPRECATED: use `commitPatchR`
commitPatch :: forall s. Versioned NgramsTablePatch -> StateCoTransformer (CoreState s) Unit
commitPatch (Versioned {version, data: tablePatch}) = do
  modifyState_ $ \s ->
    s { ngramsLocalPatch = tablePatch <> s.ngramsLocalPatch }

commitPatchR :: forall s. Versioned NgramsTablePatch -> R.State (CoreState s) -> Effect Unit
commitPatchR (Versioned {version, data: tablePatch}) (_ /\ setState) = do
  setState $ \s ->
    s { ngramsLocalPatch = tablePatch <> s.ngramsLocalPatch }

loadNgramsTable :: PageParams -> Aff VersionedNgramsTable
loadNgramsTable
  { nodeId, listIds, termListFilter, termSizeFilter, session, scoreType
  , searchQuery, tabType, params: {offset, limit, orderBy}}
  = get session query
  where query = GetNgrams { tabType, offset, limit, listIds
                          , orderBy: convOrderBy <$> orderBy
                          , termListFilter, termSizeFilter
                          , searchQuery, scoreType } (Just nodeId)

type NgramsListByTabType = Map TabType VersionedNgramsTable

loadNgramsTableAll :: PageParams -> Aff NgramsListByTabType
loadNgramsTableAll { nodeId, listIds, session, scoreType } = do
  let
    cTagNgramTypes =
      [ CTabTerms
      , CTabSources
      , CTabAuthors
      , CTabInstitutes
      ]
    query tabType = GetNgramsTableAll { tabType, listIds, scoreType } (Just nodeId)

  Map.fromFoldable <$> for cTagNgramTypes \cTagNgramType -> do
    let tabType = TabCorpus $ TabNgramType cTagNgramType
    result :: VersionedNgramsTable <- get session $ query tabType
    pure $ Tuple tabType result

convOrderBy :: T.OrderByDirection T.ColumnName -> OrderBy
convOrderBy (T.ASC  (T.ColumnName "Score")) = ScoreAsc
convOrderBy (T.DESC (T.ColumnName "Score")) = ScoreDesc
convOrderBy (T.ASC  _) = TermAsc
convOrderBy (T.DESC _) = TermDesc


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


type Dispatch = Action -> Effect Unit

isSingleNgramsTerm :: NgramsTerm -> Boolean
isSingleNgramsTerm nt = isSingleTerm $ ngramsTermText nt
  where
    isSingleTerm :: String -> Boolean
    isSingleTerm s = A.length words == 1
      where
        words = A.filter (not S.null) $ DSC.trim <$> (SU.words s)

filterTermSize :: Maybe TermSize -> NgramsTerm -> Boolean
filterTermSize (Just MonoTerm)  nt = isSingleNgramsTerm nt
filterTermSize (Just MultiTerm) nt = not $ isSingleNgramsTerm nt
filterTermSize _                _ = true
