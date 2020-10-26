module Gargantext.Components.NgramsTable.Core
  ( PageParams
  , CoreParams
  , NgramsElement(..)
  , _NgramsElement
  , NgramsRepoElement(..)
  , _NgramsRepoElement
  , ngramsRepoElementToNgramsElement
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
--, applyNgramsTablePatch -- re-export only if we have a good reason not to use applyNgramsPatches
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
  , _ngrams_repo_elements
  , _ngrams_scores
  , commitPatch
  , putNgramsPatches
  , syncPatches
  , addNewNgramP
  , addNewNgramA
  , setTermListP
  , setTermListA
  , CoreAction(..)
  , CoreDispatch
  , Action(..)
  , Dispatch
  , coreDispatch
  , isSingleNgramsTerm
  , filterTermSize
  , SyncResetButtonsProps
  , syncResetButtons
  , syncResetButtonsCpt
  )
  where

import Prelude

import Control.Monad.State (class MonadState, execState)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonEmptyObject, (.:), (.:!), (.:?), (:=), (:=?), (~>), (~>?))
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Array (head)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldlWithIndex, foldrWithIndex)
--import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Iso', Lens', use, view, (%=), (%~), (.~), (?=), (^?), (^.))
import Data.Lens.At (class At, at)
import Data.Lens.Common (_Just)
import Data.Lens.Fold (folded, traverseOf_)
import Data.Lens.Index (class Index, ix)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List ((:), List(Nil))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Set as Set
import Data.String as S
import Data.String.Common as DSC
import Data.String.Regex (Regex, regex, replace) as R
import Data.String.Regex.Flags (global, multiline) as R
import Data.String.Utils as SU
import Data.Symbol (SProxy(..))
import Data.These (These(..))
import Data.Traversable (for, traverse_)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log2)
import Effect.Aff (Aff, launchAff_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception.Unsafe (unsafeThrow)
import Foreign.Object as FO
import FFI.Simple.Functions (delay)
import Reactix as R
import Reactix.DOM.HTML as H
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)

import Gargantext.Prelude
import Gargantext.Components.Table as T
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, get, put)
import Gargantext.Types (CTabNgramType(..), OrderBy(..), ScoreType(..), TabSubType(..), TabType(..), TermList(..), TermSize(..))
import Gargantext.Utils.KarpRabin (indicesOfAny)

thisModule :: String
thisModule = "Gargantext.Components.NgramsTable.Core"

type Endo a = a -> a

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
  , termListFilter: Just MapTerm
  , searchQuery: ""
  , scoreType: Occurrences
  , session
  }
  where
    params = T.initialParams { orderBy = Just (T.DESC $ T.ColumnName "Score") }

newtype NgramsTerm = NormNgramsTerm String

derive instance genericNgramsTerm :: Generic NgramsTerm _
instance eqNgramsTerm  :: Eq NgramsTerm where
  eq = genericEq
instance ordNgramsTerm :: Ord NgramsTerm where
  compare = genericCompare
instance showNgramsTerm :: Show NgramsTerm where
  show = genericShow

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

normNgramWithTrim :: CTabNgramType -> String -> String
normNgramWithTrim nt = DSC.trim <<< normNgramInternal nt

normNgram :: CTabNgramType -> String -> NgramsTerm
normNgram tabType = NormNgramsTerm <<< normNgramWithTrim tabType

-----------------------------------------------------------------------------------
newtype NgramsElement = NgramsElement
  { ngrams      :: NgramsTerm -- HERE
  , size        :: Int -- MISSING
  , list        :: TermList -- ok
  , root        :: Maybe NgramsTerm -- ok
  , parent      :: Maybe NgramsTerm -- ok
  , children    :: Set NgramsTerm -- ok
  , occurrences :: Int -- HERE
  }

derive instance eqNgramsElement :: Eq NgramsElement


_parent :: forall parent row. Lens' { parent :: parent | row } parent
_parent = prop (SProxy :: SProxy "parent")

_root :: forall root row. Lens' { root :: root | row } root
_root   = prop (SProxy :: SProxy "root")

_ngrams :: forall row. Lens' { ngrams :: NgramsTerm | row } NgramsTerm
_ngrams = prop (SProxy :: SProxy "ngrams")

_children :: forall row. Lens' { children :: Set NgramsTerm | row } (Set NgramsTerm)
_children = prop (SProxy :: SProxy "children")

_occurrences :: forall row. Lens' { occurrences :: Int | row } Int
_occurrences = prop (SProxy :: SProxy "occurrences")

_list :: forall a row. Lens' { list :: a | row } a
_list = prop (SProxy :: SProxy "list")

_ngrams_repo_elements :: forall a row. Lens' { ngrams_repo_elements :: a | row } a
_ngrams_repo_elements = prop (SProxy :: SProxy "ngrams_repo_elements")

_ngrams_scores :: forall a row. Lens' { ngrams_scores :: a | row } a
_ngrams_scores = prop (SProxy :: SProxy "ngrams_scores")

derive instance newtypeNgramsElement :: Newtype NgramsElement _
derive instance genericNgramsElement :: Generic NgramsElement _
instance showNgramsElement :: Show NgramsElement where
  show = genericShow

_NgramsElement  :: Iso' NgramsElement {
    children    :: Set NgramsTerm
  , size        :: Int
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
    size        <- obj .:  "size"
    list        <- obj .:  "list"
    occurrences <- obj .:  "occurrences"
    parent      <- obj .:? "parent"
    root        <- obj .:? "root"
    children'   <- obj .:  "children"
    let children = Set.fromFoldable (children' :: Array NgramsTerm)
    pure $ NgramsElement {ngrams, size, list, occurrences, parent, root, children}

instance encodeJsonNgramsElement :: EncodeJson NgramsElement where
  encodeJson (NgramsElement { children, list, ngrams, occurrences, parent, root }) = 
       "children"    := children
    ~> "list"        := list
    ~> "ngrams"      := ngrams
    ~> "occurrences" := occurrences
    ~> "parent"      :=? parent
    ~>? "root"        :=? root
    ~>? jsonEmptyObject

newtype NgramsRepoElement = NgramsRepoElement
  { size     :: Int
  , list     :: TermList
  , root     :: Maybe NgramsTerm
  , parent   :: Maybe NgramsTerm
  , children :: Set NgramsTerm
--  , occurrences :: Int -- TODO
  }

derive instance eqNgramsRepoElement  :: Eq NgramsRepoElement

instance decodeJsonNgramsRepoElement :: DecodeJson NgramsRepoElement where
  decodeJson json = do
    obj         <- decodeJson json
    size        <- obj .:  "size"
    list        <- obj .:  "list"
    parent      <- obj .:? "parent"
    root        <- obj .:? "root"
    children'   <- obj .:  "children"
    let children = Set.fromFoldable (children' :: Array NgramsTerm)
    pure $ NgramsRepoElement {size, list, parent, root, children}

instance encodeJsonNgramsRepoElement :: EncodeJson NgramsRepoElement where
  encodeJson (NgramsRepoElement { size, list, root, parent, children {-occurrences-} })
     =  "size"       :=  size
    ~>  "list"       :=  list
    ~>  "root"       :=? root
    ~>? "parent"     :=? parent
    ~>? "children"   :=  children
--    ~>  "occurrences" := occurrences
    ~>  jsonEmptyObject

derive instance newtypeNgramsRepoElement :: Newtype NgramsRepoElement _
derive instance genericNgramsRepoElement :: Generic NgramsRepoElement _
instance showNgramsRepoElement :: Show NgramsRepoElement where
  show = genericShow

_NgramsRepoElement  :: Iso' NgramsRepoElement {
    children    :: Set NgramsTerm
  , size        :: Int
  , list        :: TermList
  , parent      :: Maybe NgramsTerm
  , root        :: Maybe NgramsTerm
--  , occurrences :: Int
  }
_NgramsRepoElement = _Newtype

ngramsRepoElementToNgramsElement :: NgramsTerm -> Int -> NgramsRepoElement -> NgramsElement
ngramsRepoElementToNgramsElement ngrams occurrences (NgramsRepoElement { size, list, root, parent, children }) =
  NgramsElement
  { ngrams
  , size -- TODO should we assert that size(ngrams) == size?
  , list
  , root
  , parent
  , children
  , occurrences
  }

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

{-
  NgramsRepoElement does not have the occurrences field.
  Instead NgramsTable has a ngrams_scores map.

  Pro:
  * Does not encumber NgramsRepoElement with the score which is not part of repo.
  * Enables for multiple scores through multiple maps.
  Cons:
  * Having a map on the side is equivalent to a `occurrences :: Maybe Int`, which is
    less precise.
  * It is a tiny bit less performant to access the score.
-}
newtype NgramsTable = NgramsTable
  { ngrams_repo_elements :: Map NgramsTerm NgramsRepoElement
  , ngrams_scores        :: Map NgramsTerm (Additive Int)
  }

derive instance newtypeNgramsTable :: Newtype NgramsTable _
derive instance genericNgramsTable :: Generic NgramsTable _
instance eqNgramsTable  :: Eq NgramsTable where
  eq = genericEq
instance showNgramsTable :: Show NgramsTable where
  show = genericShow

_NgramsTable :: Iso' NgramsTable
                     { ngrams_repo_elements :: Map NgramsTerm NgramsRepoElement
                     , ngrams_scores        :: Map NgramsTerm (Additive Int)
                     }
_NgramsTable = _Newtype

instance indexNgramsTable :: Index NgramsTable NgramsTerm NgramsRepoElement where
  ix k = _NgramsTable <<< _ngrams_repo_elements <<< ix k

instance atNgramsTable :: At NgramsTable NgramsTerm NgramsRepoElement where
  at k = _NgramsTable <<< _ngrams_repo_elements <<< at k

instance decodeJsonNgramsTable :: DecodeJson NgramsTable where
  decodeJson json = do
    elements <- decodeJson json
    pure $ NgramsTable
      { ngrams_repo_elements: Map.fromFoldable $ f <$> (elements :: Array NgramsElement)
      , ngrams_scores:        Map.fromFoldable $ g <$> elements
      }
    where
      f (NgramsElement {ngrams, size, list, root, parent, children}) =
        Tuple ngrams (NgramsRepoElement {size, list, root, parent, children})
      g (NgramsElement e) = Tuple e.ngrams (Additive e.occurrences)

{- NOT USED
instance encodeJsonNgramsTable :: EncodeJson NgramsTable where
  encodeJson (NgramsTable {ngrams_repo_elements, ngrams_scores}) = encodeJson $ Map.values ... TODO
-}
-----------------------------------------------------------------------------------

lookupRootList :: NgramsTerm -> NgramsTable -> Maybe TermList
lookupRootList ngram (NgramsTable {ngrams_repo_elements: elts}) =
  case Map.lookup ngram elts of
    Nothing -> Nothing
    Just (NgramsRepoElement {list, root: Nothing}) -> Just list
    Just (NgramsRepoElement {root: Just root}) ->
      case Map.lookup root elts of
        Nothing -> Nothing
        Just (NgramsRepoElement {list}) -> Just list -- assert root == Nothing

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
highlightNgrams ntype table@(NgramsTable {ngrams_repo_elements: elts}) input0 =
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
    pats = A.fromFoldable (Map.keys elts)
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
              case lookupRootList pat table of
                Nothing ->
                  crashWith "highlightNgrams: pattern missing from table"
                Just ne_list ->
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
                  , l:  Tuple (undb s2.before) (Just ne_list) :
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
      _                           -> Left $ TypeMismatch "decodeJsonReplace"

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

data NgramsPatch
  = NgramsReplace
      { patch_old :: Maybe NgramsRepoElement
      , patch_new :: Maybe NgramsRepoElement
      }
  | NgramsPatch
      { patch_children :: PatchSet NgramsTerm
      , patch_list     :: Replace TermList
      }

-- TODO shall we normalise as in replace? shall we make a type class Replaceable?
ngramsReplace :: Maybe NgramsRepoElement -> Maybe NgramsRepoElement -> NgramsPatch
ngramsReplace patch_old patch_new = NgramsReplace {patch_old, patch_new}

derive instance eqNgramsPatch  :: Eq NgramsPatch
derive instance eqPatchSetNgramsTerm  :: Eq (PatchSet NgramsTerm)

-- TODO
invert :: forall a. a -> a
invert _ = unsafeThrow "invert: TODO"

instance semigroupNgramsPatch :: Semigroup NgramsPatch where
  append (NgramsReplace p) (NgramsReplace q)
    | p.patch_old /= q.patch_new = unsafeThrow "append/NgramsPatch: old != new"
    | otherwise                  = ngramsReplace q.patch_old p.patch_new
  append (NgramsPatch p)   (NgramsPatch q) = NgramsPatch
    { patch_children: p.patch_children <> q.patch_children
    , patch_list:     p.patch_list     <> q.patch_list
    }
  append (NgramsPatch p) (NgramsReplace q) = ngramsReplace q.patch_old (q.patch_new # _Just <<< _Newtype %~ applyNgramsPatch' p)
  append (NgramsReplace p) (NgramsPatch q) = ngramsReplace (p.patch_old # _Just <<< _Newtype %~ applyNgramsPatch' (invert q)) p.patch_new

instance monoidNgramsPatch :: Monoid NgramsPatch where
  mempty = NgramsPatch { patch_children: mempty, patch_list: mempty }

instance encodeJsonNgramsPatch :: EncodeJson NgramsPatch where
  encodeJson (NgramsReplace { patch_old, patch_new })
     = "patch_old" := patch_old
    ~> "patch_new" := patch_new
    ~> jsonEmptyObject
  encodeJson (NgramsPatch { patch_children, patch_list })
  -- TODO only include non empty fields
     = "patch_children" := patch_children
    ~> "patch_list"     := patch_list
    ~> jsonEmptyObject

instance decodeJsonNgramsPatch :: DecodeJson NgramsPatch where
  decodeJson json = do
    obj            <- decodeJson json
    -- TODO handle empty fields
    -- TODO handle patch_new
    patch_new <- obj .:? "patch_new"
    patch_old <- obj .:? "patch_old"
    if isJust patch_new || isJust patch_old then
      pure $ NgramsReplace { patch_old, patch_new }
    else do
      patch_list     <- obj .: "patch_list"
      patch_children <- obj .: "patch_children"
      pure $ NgramsPatch { patch_list, patch_children }

applyNgramsPatch' :: forall row.
                          { patch_children :: PatchSet NgramsTerm
                          , patch_list     :: Replace TermList
                          } ->
                     Endo { list     :: TermList
                          , children :: Set NgramsTerm
                          | row
                          }
applyNgramsPatch' p e =
  e { list     = applyReplace p.patch_list e.list
    , children = applyPatchSet p.patch_children e.children
    }

applyNgramsPatch :: NgramsPatch -> Maybe NgramsRepoElement -> Maybe NgramsRepoElement
applyNgramsPatch (NgramsReplace {patch_new}) _ = patch_new
applyNgramsPatch (NgramsPatch p)           m = m # _Just <<< _Newtype %~ applyNgramsPatch' p

newtype PatchMap k p = PatchMap (Map k p)

fromMap :: forall k p. Ord k => Eq p => Monoid p => Map k p -> PatchMap k p
fromMap = PatchMap <<< Map.filter (\v -> v /= mempty)

instance semigroupPatchMap :: (Ord k, Eq p, Monoid p) => Semigroup (PatchMap k p) where
  append (PatchMap p) (PatchMap q) = fromMap $ Map.unionWith append p q

instance monoidPatchMap :: (Ord k, Eq p, Monoid p) => Monoid (PatchMap k p) where
  mempty = PatchMap Map.empty

derive instance newtypePatchMap :: Newtype (PatchMap k p) _
derive instance eqPatchMap :: (Eq k, Eq p) => Eq (PatchMap k p)

_PatchMap :: forall k p. Iso' (PatchMap k p) (Map k p)
_PatchMap = _Newtype

{-
instance functorPatchMap :: Functor (PatchMap k) where
  map f (PatchMap m) = PatchMap (map f m) -- NO NORM: fromMap would not typecheck

instance functorWithIndexPatchMap :: FunctorWithIndex k (PatchMap k) where
  mapWithIndex f (PatchMap m) = PatchMap (mapWithIndex f m) -- NO NORM: fromMap would not typecheck
-}

instance foldlablePatchMap :: Foldable (PatchMap k) where
  foldr f z (PatchMap m) = foldr f z m
  foldl f z (PatchMap m) = foldl f z m
  foldMap f (PatchMap m) = foldMap f m

instance foldlableWithIndexPatchMap :: FoldableWithIndex k (PatchMap k) where
  foldrWithIndex f z (PatchMap m) = foldrWithIndex f z m
  foldlWithIndex f z (PatchMap m) = foldlWithIndex f z m
  foldMapWithIndex f (PatchMap m) = foldMapWithIndex f m

{- fromMap is preventing these to type check:

instance traversablePatchMap :: Ord k => Traversable (PatchMap k) where
  traverse f (PatchMap m) = fromMap <$> traverse f m
  sequence (PatchMap m) = fromMap <$> sequence m

instance traversableWithIndexPatchMap :: Ord k => TraversableWithIndex k (PatchMap k) where
  traverseWithIndex f (PatchMap m) = fromMap <$> traverseWithIndex f m
-}

traversePatchMapWithIndex :: forall f a b k.
                             Applicative f => Ord k => Eq b => Monoid b =>
                             (k -> a -> f b) -> PatchMap k a -> f (PatchMap k b)
traversePatchMapWithIndex f (PatchMap m) = fromMap <$> traverseWithIndex f m

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

mergeMap :: forall k a b c. Ord k => (k -> These a b -> Maybe c) -> Map k a -> Map k b -> Map k c
mergeMap f m1 m2 = Map.mapMaybeWithKey f (Map.unionWith g (This <$> m1) (That <$> m2))
  where
    g (This p) (That v) = Both p v
    g x _ = x -- impossible

applyPatchMap :: forall k p v. Ord k => (p -> Maybe v -> Maybe v) -> PatchMap k p -> Map k v -> Map k v
applyPatchMap applyPatchValue (PatchMap pm) m = mergeMap f pm m
  where
    f _ (This pv)   = applyPatchValue pv Nothing
    f _ (That v)    = Just v
    f _ (Both pv v) = applyPatchValue pv (Just v)

type NgramsPatches = PatchMap NgramsTerm NgramsPatch
type VersionedNgramsPatches = Versioned NgramsPatches

type NewElems = Map NgramsTerm TermList

-- TODO replace by NgramsPatches directly
type NgramsTablePatch = { ngramsPatches :: NgramsPatches }

isEmptyNgramsTablePatch :: NgramsTablePatch -> Boolean
isEmptyNgramsTablePatch {ngramsPatches} = isEmptyPatchMap ngramsPatches

fromNgramsPatches :: NgramsPatches -> NgramsTablePatch
fromNgramsPatches ngramsPatches = {ngramsPatches}

findNgramTermList :: NgramsTable -> NgramsTerm -> Maybe TermList
findNgramTermList (NgramsTable m) n = m.ngrams_repo_elements ^? at n <<< _Just <<< _NgramsRepoElement <<< _list

singletonNgramsTablePatch :: NgramsTerm -> NgramsPatch -> NgramsTablePatch
singletonNgramsTablePatch n p = fromNgramsPatches $ singletonPatchMap n p

rootsOf :: NgramsTable -> Set NgramsTerm
rootsOf (NgramsTable m) = Map.keys $ Map.mapMaybe isRoot m.ngrams_repo_elements
  where
    isRoot (NgramsRepoElement { parent }) = parent

type RootParent = { root :: NgramsTerm, parent :: NgramsTerm }

type ReParent a = forall m. MonadState NgramsTable m => a -> m Unit

reRootMaxDepth :: Int
reRootMaxDepth = 100 -- TODO: this is a hack

reRootChildren :: Int -> NgramsTerm -> ReParent NgramsTerm
reRootChildren 0         _    _     = pure unit -- TODO: this is a hack
reRootChildren max_depth root ngram = do
  nre <- use (at ngram)
  traverseOf_ (_Just <<< _NgramsRepoElement <<< _children <<< folded) (\child -> do
    at child <<< _Just <<< _NgramsRepoElement <<< _root ?= root
    reRootChildren (max_depth - 1) root child) nre

reParent :: Maybe RootParent -> ReParent NgramsTerm
reParent mrp child = do
  at child <<< _Just <<< _NgramsRepoElement %= ((_parent .~ (view _parent <$> mrp)) <<<
                                                (_root   .~ (view _root   <$> mrp)))
  reRootChildren reRootMaxDepth (fromMaybe child (mrp ^? _Just <<< _root)) child

-- reParentNgramsPatch :: NgramsTerm -> ReParent NgramsPatch
-- ^ GHC would have accepted this type. Here reParentNgramsPatch checks but
--   not its usage in reParentNgramsTablePatch.
reParentNgramsPatch :: forall m. MonadState NgramsTable m
                    => NgramsTerm -> NgramsPatch -> m Unit
reParentNgramsPatch _      (NgramsReplace _) = pure unit -- TODO
reParentNgramsPatch parent (NgramsPatch {patch_children: PatchSet {rem, add}}) = do
  -- root_of_parent <- use (at parent <<< _Just <<< _NgramsElement <<< _root)
  -- ^ TODO this does not type checks, we do the following two lines instead:
  s <- use (at parent)
  let root_of_parent = s ^? (_Just <<< _NgramsRepoElement <<< _root <<< _Just)
  let rp = { root: fromMaybe parent root_of_parent, parent }
  traverse_ (reParent Nothing) rem
  traverse_ (reParent $ Just rp) add

reParentNgramsTablePatch :: ReParent NgramsPatches
reParentNgramsTablePatch = void <<< traversePatchMapWithIndex reParentNgramsPatch

{-
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
-}

applyNgramsTablePatch :: NgramsTablePatch -> NgramsTable -> NgramsTable
applyNgramsTablePatch { ngramsPatches } (NgramsTable m) =
  execState (reParentNgramsTablePatch ngramsPatches) $
  NgramsTable $ m { ngrams_repo_elements =
                      applyPatchMap applyNgramsPatch ngramsPatches m.ngrams_repo_elements }

applyNgramsPatches :: forall s. CoreState s -> NgramsTable -> NgramsTable
applyNgramsPatches {ngramsLocalPatch, ngramsStagePatch, ngramsValidPatch} =
  applyNgramsTablePatch (ngramsLocalPatch <> ngramsStagePatch <> ngramsValidPatch)
  -- First the valid patch, then the stage patch, and finally the local patch.
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

{-
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
-}

newNgramPatch :: TermList -> NgramsPatch
newNgramPatch list =
  NgramsReplace
  { patch_old: Nothing
  , patch_new:
      Just $ NgramsRepoElement
      { size: 1 -- TODO
      , list
      , root:     Nothing
      , parent:   Nothing
      , children: mempty
      -- , occurrences: 0 -- TODO
      }
  }

addNewNgramP :: NgramsTerm -> TermList -> NgramsTablePatch
addNewNgramP ngrams list =
  { ngramsPatches: singletonPatchMap ngrams (newNgramPatch list) }

addNewNgramA :: NgramsTerm -> TermList -> CoreAction
addNewNgramA ngrams list = CommitPatch $ addNewNgramP ngrams list

setTermListP :: NgramsTerm -> Replace TermList -> NgramsTablePatch
setTermListP ngram patch_list = singletonNgramsTablePatch ngram pe
  where
    pe = NgramsPatch { patch_list, patch_children: mempty }

setTermListA :: NgramsTerm -> Replace TermList -> CoreAction
setTermListA ngram termList = CommitPatch $ setTermListP ngram termList

putNgramsPatches :: forall s. CoreParams s -> VersionedNgramsPatches -> Aff VersionedNgramsPatches
putNgramsPatches {session, nodeId, listIds, tabType} = put session putNgrams
  where putNgrams = PutNgrams tabType (head listIds) Nothing (Just nodeId)

syncPatches :: forall p s. CoreParams p -> R.State (CoreState s) -> (Unit -> Aff Unit) -> Effect Unit
syncPatches props ({ ngramsLocalPatch: ngramsLocalPatch@{ ngramsPatches }
                   , ngramsStagePatch
                   , ngramsValidPatch
                   , ngramsVersion
                   } /\ setState) callback = do
  when (isEmptyNgramsTablePatch ngramsStagePatch) $ do
    -- setState $ \s ->
    --   s { ngramsLocalPatch = fromNgramsPatches mempty
    --     , ngramsStagePatch = ngramsLocalPatch
    --     }
    let pt = Versioned { data: ngramsPatches, version: ngramsVersion }
    launchAff_ $ do
      Versioned { data: newPatch, version: newVersion } <- putNgramsPatches props pt
      callback unit
      liftEffect $ do
        log2 "[syncPatches] setting state, newVersion" newVersion
        setState $ \s ->
          -- I think that sometimes this setState does not fully go through.
          -- This is an issue because the version number does not get updated and the subsequent calls
          -- can mess up the patches.
          s {
              ngramsLocalPatch = fromNgramsPatches mempty
            , ngramsStagePatch = fromNgramsPatches mempty
            , ngramsValidPatch = fromNgramsPatches newPatch <> ngramsLocalPatch <> s.ngramsValidPatch
                              -- First the already valid patch, then the local patch, then the newly received newPatch.
            , ngramsVersion    = newVersion
            }
        log2 "[syncPatches] ngramsVersion" newVersion

commitPatch :: forall s. Versioned NgramsTablePatch -> R.State (CoreState s) -> Effect Unit
commitPatch (Versioned {version, data: tablePatch}) (_ /\ setState) = do
  setState $ \s ->
    s { ngramsLocalPatch = tablePatch <> s.ngramsLocalPatch }
    -- First we apply the patches we have locally and then the new patch (tablePatch).

loadNgramsTable :: PageParams -> Aff VersionedNgramsTable
loadNgramsTable
  { nodeId, listIds, termListFilter, termSizeFilter, session, scoreType
  , searchQuery, tabType, params: {offset, limit, orderBy}}
  = get session query
    where
      query = GetNgramsTableAll { listIds
                                , tabType } (Just nodeId)
  -- where query = GetNgrams { limit
  --                         , offset: Just offset
  --                         , listIds
  --                         , orderBy: convOrderBy <$> orderBy
  --                         , searchQuery
  --                         , tabType
  --                         , termListFilter
  --                         , termSizeFilter } (Just nodeId)

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
    query tabType = GetNgramsTableAll { listIds, tabType } (Just nodeId)

  Map.fromFoldable <$> for cTagNgramTypes \cTagNgramType -> do
    let tabType = TabCorpus $ TabNgramType cTagNgramType
    result :: VersionedNgramsTable <- get session $ query tabType
    pure $ Tuple tabType result

convOrderBy :: T.OrderByDirection T.ColumnName -> OrderBy
convOrderBy (T.ASC  (T.ColumnName "Score")) = ScoreAsc
convOrderBy (T.DESC (T.ColumnName "Score")) = ScoreDesc
convOrderBy (T.ASC  _) = TermAsc
convOrderBy (T.DESC _) = TermDesc

data CoreAction
  = CommitPatch NgramsTablePatch
  | Synchronize { afterSync :: Unit -> Aff Unit }
  | ResetPatches

data Action
  = CoreAction CoreAction
  | SetParentResetChildren (Maybe NgramsTerm)
  -- ^ This sets `ngramsParent` and resets `ngramsChildren`.
  | ToggleChild Boolean NgramsTerm
  -- ^ Toggles the NgramsTerm in the `PatchSet` `ngramsChildren`.
  -- If the `Boolean` is `true` it means we want to add it if it is not here,
  -- if it is `false` it is meant to be removed if not here.
  | AddTermChildren
  | ToggleSelect NgramsTerm
  -- ^ Toggles the NgramsTerm in the `Set` `ngramsSelection`.
  | ToggleSelectAll


type CoreDispatch = CoreAction -> Effect Unit
type Dispatch = Action -> Effect Unit

coreDispatch :: forall p s. CoreParams p -> R.State (CoreState s) -> CoreDispatch
coreDispatch path state (Synchronize { afterSync }) =
  syncPatches path state afterSync
coreDispatch _ state@({ngramsVersion} /\ _) (CommitPatch pt) =
  commitPatch (Versioned {version: ngramsVersion, data: pt}) state
coreDispatch _ (_ /\ setState) ResetPatches =
  setState $ \s -> s { ngramsLocalPatch = { ngramsPatches: mempty } }

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
filterTermSize _                _  = true

type SyncResetButtonsProps =
  ( afterSync :: Unit -> Aff Unit
  , ngramsLocalPatch :: NgramsTablePatch
  , performAction :: CoreDispatch
  )

syncResetButtons :: Record SyncResetButtonsProps -> R.Element
syncResetButtons p = R.createElement syncResetButtonsCpt p []

syncResetButtonsCpt :: R.Component SyncResetButtonsProps
syncResetButtonsCpt = R.hooksComponentWithModule thisModule "syncResetButtons" cpt
  where
    cpt { afterSync, ngramsLocalPatch, performAction } _ = do
      synchronizing@(s /\ setSynchronizing) <- R.useState' false

      let
        hasChanges = ngramsLocalPatch /= mempty

        newAfterSync x = do
          afterSync x
          liftEffect $ setSynchronizing $ const false

        synchronizeClick _ = delay unit $ \_ -> do
          setSynchronizing $ const true
          performAction $ Synchronize { afterSync: newAfterSync }

      pure $ H.div {} [
        H.button { className: "btn btn-danger " <> if hasChanges then "" else " disabled"
                 , on: { click: \_ -> performAction ResetPatches }
                 } [ H.text "Reset" ]
      , H.button { className: "btn btn-primary " <> (if s || (not hasChanges) then "disabled" else "")
                 , on: { click: synchronizeClick }
                 } [ H.text "Sync" ]
        ]