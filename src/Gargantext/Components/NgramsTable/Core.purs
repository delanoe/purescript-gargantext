module Gargantext.Components.NgramsTable.Core
  ( PageParams
  , CoreParams
  , NgramsElement(..)
  , _NgramsElement
  , NgramsRepoElementT
  , NgramsRepoElement(..)
  , _NgramsRepoElement
  , ngramsRepoElementToNgramsElement
  , NgramsTable(..)
  , NewElems
  , NgramsPatch(..)
  , NgramsPatches
  , _NgramsTable
  , NgramsTerm(..)
  , normNgram
  , ngramsTermText
  , findNgramRoot
  , findNgramTermList
  , Version
  , Versioned(..)
  , Count
  , VersionedWithCount(..)
  , toVersioned
  , VersionedNgramsPatches
  , AsyncNgramsChartsUpdate(..)
  , VersionedNgramsTable
  , VersionedWithCountNgramsTable
  , NgramsTablePatch
  , CoreState
  , HighlightElement
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
  , postNgramsChartsAsync
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

  -- Reset Button TODO put elsewhere this file is too big
  , SyncResetButtonsProps
  , syncResetButtons
  , chartsAfterSync
  )
  where

import Control.Monad.State (class MonadState, execState)
import Data.Array (head)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldlWithIndex, foldrWithIndex)
--import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.Lens (Iso', Lens', use, view, (%=), (%~), (.~), (?=), (^?))
import Data.Lens.At (class At, at)
import Data.Lens.Common (_Just)
import Data.Lens.Fold (folded, traverseOf_)
import Data.Lens.Index (class Index, ix)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List ((:), List(Nil))
import Data.List as L
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe', isJust)
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
import Data.Traversable (for, traverse_, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import DOM.Simple.Console (log2)
import Effect.Aff (Aff, launchAff_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception.Unsafe (unsafeThrow)
import Foreign as F
import Foreign.Object as FO
import FFI.Simple.Functions (delay)
import Reactix (Component, Element, createElement) as R
import Reactix.DOM.HTML as H
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)
import Simple.JSON as JSON
import Toestand (Box, modify_, read, unequal, useBox, useLive, write_) as T

import Gargantext.Prelude

import Gargantext.AsyncTasks as GAT
import Gargantext.Components.Table (initialParams) as T
import Gargantext.Components.Table.Types (ColumnName(..), OrderByDirection(..), Params) as T
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, get, post, put)
import Gargantext.Types (AsyncTaskType(..), AsyncTaskWithType(..), CTabNgramType(..), ListId, OrderBy(..), ScoreType(..), TabSubType(..), TabType(..), TermList(..), TermSize(..))
import Gargantext.Utils.KarpRabin (indicesOfAny)
import Gargantext.Utils.Reactix as R2
  
here :: R2.Here
here = R2.here "Gargantext.Components.NgramsTable.Core"

type Endo a = a -> a


-- | Main Types
type Version = Int

newtype Versioned a = Versioned
  { version :: Version
  , data    :: a
  }
derive instance Generic (Versioned a) _
derive instance Newtype (Versioned a) _
instance Eq a => Eq (Versioned a) where eq = genericEq
derive newtype instance JSON.ReadForeign a => JSON.ReadForeign (Versioned a)
derive newtype instance JSON.WriteForeign a => JSON.WriteForeign (Versioned a)
------------------------------------------------------------------------
type Count = Int

newtype VersionedWithCount a = VersionedWithCount
  { version :: Version
  , count   :: Count
  , data    :: a
  }
derive instance Generic (VersionedWithCount a) _
derive instance Newtype (VersionedWithCount a) _
instance Eq a => Eq (VersionedWithCount a) where eq = genericEq
derive newtype instance JSON.ReadForeign a => JSON.ReadForeign (VersionedWithCount a)
derive newtype instance JSON.WriteForeign a => JSON.WriteForeign (VersionedWithCount a)

toVersioned :: forall a. VersionedWithCount a -> Tuple Count (Versioned a)
toVersioned (VersionedWithCount { count, data: d, version }) = Tuple count $ Versioned { data: d, version }

------------------------------------------------------------------------
-- TODO replace by NgramsPatches directly
type NgramsTablePatch = { ngramsPatches :: NgramsPatches }

newtype PatchMap k p = PatchMap (Map k p)

derive instance Generic (PatchMap k p) _
derive instance Newtype (PatchMap k p) _
derive instance (Eq k, Eq p) => Eq (PatchMap k p)

-- TODO generalize
instance JSON.WriteForeign p => JSON.WriteForeign (PatchMap NgramsTerm p) where
  writeImpl (PatchMap m) =
    JSON.writeImpl $ FO.fromFoldable $ map (lmap ngramsTermText) (Map.toUnfoldable m :: Array _)
instance (JSON.ReadForeign p, Monoid p) => JSON.ReadForeign (PatchMap NgramsTerm p) where
  readImpl f = do
    inst <- JSON.readImpl f
    pure $ PatchMap $ foldlWithIndex (\k m v -> Map.insert (NormNgramsTerm k) v m) Map.empty (inst :: FO.Object p)
    -- TODO we assume that the ngrams are already normalized ^^^^^^^^^^^^^

type NgramsPatches = PatchMap NgramsTerm NgramsPatch

data NgramsPatch
  = NgramsReplace
      { patch_old :: Maybe NgramsRepoElement
      , patch_new :: Maybe NgramsRepoElement
      }
  | NgramsPatch
      { patch_children :: PatchSet NgramsTerm
      , patch_list     :: Replace TermList
      }
derive instance Generic NgramsPatch _
derive instance Eq NgramsPatch
instance Monoid NgramsPatch where
  mempty = NgramsPatch { patch_children: mempty, patch_list: mempty }
instance Semigroup NgramsPatch where
  append (NgramsReplace p) (NgramsReplace q)
    | p.patch_old /= q.patch_new = unsafeThrow "append/NgramsPatch: old != new"
    | otherwise                  = ngramsReplace q.patch_old p.patch_new
  append (NgramsPatch p)   (NgramsPatch q) = NgramsPatch
    { patch_children: p.patch_children <> q.patch_children
    , patch_list:     p.patch_list     <> q.patch_list
    }
  append (NgramsPatch p) (NgramsReplace q) = ngramsReplace q.patch_old (q.patch_new # _Just <<< _Newtype %~ applyNgramsPatch' p)
  append (NgramsReplace p) (NgramsPatch q) = ngramsReplace (p.patch_old # _Just <<< _Newtype %~ applyNgramsPatch' (invert q)) p.patch_new
instance JSON.WriteForeign NgramsPatch where
  writeImpl (NgramsReplace { patch_old, patch_new }) = JSON.writeImpl { patch_old, patch_new }
  writeImpl (NgramsPatch { patch_children, patch_list }) = JSON.writeImpl { patch_children, patch_list }
instance JSON.ReadForeign NgramsPatch where
  readImpl f = do
    inst :: { patch_old :: Maybe NgramsRepoElement
            , patch_new :: Maybe NgramsRepoElement
            , patch_children :: PatchSet NgramsTerm
            , patch_list :: Replace TermList } <- JSON.readImpl f
    -- TODO handle empty fields
    -- TODO handle patch_new
    if isJust inst.patch_new || isJust inst.patch_old then
      pure $ NgramsReplace { patch_old: inst.patch_old, patch_new: inst.patch_new }
    else do
      pure $ NgramsPatch { patch_list: inst.patch_list, patch_children: inst.patch_children }

------------------------------------------------------------------------
newtype NgramsTerm = NormNgramsTerm String
derive instance Generic NgramsTerm _
derive instance Newtype NgramsTerm _
instance Eq NgramsTerm where eq = genericEq
instance Ord NgramsTerm where compare = genericCompare
instance Show NgramsTerm where show = genericShow
derive newtype instance JSON.ReadForeign NgramsTerm
derive newtype instance JSON.WriteForeign NgramsTerm
derive newtype instance Monoid NgramsTerm

------------------------------------------------------------------------

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
  { listIds
  , nodeId
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

derive instance Eq NgramsElement


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

derive instance Newtype NgramsElement _
derive instance Generic NgramsElement _
instance Show NgramsElement where show = genericShow

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

instance JSON.ReadForeign NgramsElement where
  readImpl f = do
    inst :: { children :: Array NgramsTerm
            , size :: Int
            , list :: TermList
            , ngrams :: NgramsTerm
            , occurrences :: Int
            , parent :: Maybe NgramsTerm
            , root :: Maybe NgramsTerm }<- JSON.readImpl f
    pure $ NgramsElement $ inst { children = Set.fromFoldable inst.children }
instance JSON.WriteForeign NgramsElement where
  writeImpl (NgramsElement ne) =
    JSON.writeImpl $ ne { children = Set.toUnfoldable ne.children :: Array _ }

type NgramsRepoElementT =
  ( size :: Int
  , list     :: TermList
  , root     :: Maybe NgramsTerm
  , parent   :: Maybe NgramsTerm
  )
newtype NgramsRepoElement = NgramsRepoElement
  { children :: Set NgramsTerm
  | NgramsRepoElementT }
derive instance Generic NgramsRepoElement _
derive instance Newtype NgramsRepoElement _
derive instance Eq NgramsRepoElement
instance JSON.ReadForeign NgramsRepoElement where
  readImpl f = do
    inst :: { children :: Array NgramsTerm | NgramsRepoElementT } <- JSON.readImpl f
    pure $ NgramsRepoElement $ inst { children = Set.fromFoldable inst.children }
instance JSON.WriteForeign NgramsRepoElement where
  writeImpl (NgramsRepoElement nre) =
    JSON.writeImpl $ nre { children = Set.toUnfoldable nre.children :: Array _ }
instance Show NgramsRepoElement where show = genericShow

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
ngramsRepoElementToNgramsElement ngrams occurrences (NgramsRepoElement { children, list, parent, root, size }) =
  NgramsElement
  { children
  , list
  , ngrams
  , occurrences
  , parent
  , root
  , size -- TODO should we assert that size(ngrams) == size?
  }

-----------------------------------------------------------------------------------
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

derive instance Newtype NgramsTable _
derive instance Generic NgramsTable _
instance Eq NgramsTable where eq = genericEq
instance Show NgramsTable where show = genericShow

_NgramsTable :: Iso' NgramsTable
                     { ngrams_repo_elements :: Map NgramsTerm NgramsRepoElement
                     , ngrams_scores        :: Map NgramsTerm (Additive Int)
                     }
_NgramsTable = _Newtype

instance Index NgramsTable NgramsTerm NgramsRepoElement where
  ix k = _NgramsTable <<< _ngrams_repo_elements <<< ix k

instance At NgramsTable NgramsTerm NgramsRepoElement where
  at k = _NgramsTable <<< _ngrams_repo_elements <<< at k

instance JSON.ReadForeign NgramsTable where
  readImpl ff = do
    inst <- JSON.readImpl ff
    pure $ NgramsTable
      { ngrams_repo_elements: Map.fromFoldable $ f <$> (inst :: Array NgramsElement)
      , ngrams_scores:        Map.fromFoldable $ g <$> inst
      }
    where
      f (NgramsElement {ngrams, size, list, root, parent, children}) =
        Tuple ngrams (NgramsRepoElement {size, list, root, parent, children})
      g (NgramsElement e) = Tuple e.ngrams (Additive e.occurrences)

{- NOT USED
instance EncodeJson NgramsTable where
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

type HighlightElement = Tuple String (List (Tuple NgramsTerm TermList))
type HighlightAccumulator = List HighlightElement

-- TODO: while this function works well with word boundaries,
--       it inserts too many spaces.
highlightNgrams :: CTabNgramType -> NgramsTable -> String -> Array HighlightElement
highlightNgrams ntype table@(NgramsTable {ngrams_repo_elements: elts}) input0 =
    -- trace {pats, input0, input, ixs} \_ ->
    A.fromFoldable ((\(s /\ ls)-> undb s /\ ls) <$> unsafePartial (foldl goFold ((input /\ Nil) : Nil) ixs))
  where
    spR x = " " <> R.replace wordBoundaryReg "$1$1" x <> " "
    reR = R.replace wordBoundaryReg " "
    db = S.replaceAll (S.Pattern " ") (S.Replacement "  ")
    sp x = " " <> db x <> " "
    undb = R.replace wordBoundaryReg2 "$1"
    input = spR input0
    pats = A.fromFoldable (Map.keys elts)
    ixs = indicesOfAny (sp <<< ngramsTermText <$> pats) (normNgramInternal ntype input)

    splitAcc :: Partial => Int -> HighlightAccumulator
             -> Tuple HighlightAccumulator HighlightAccumulator
    splitAcc i = go 0 Nil
      where
      go j pref acc =
        case compare i j of
          LT -> crashWith "highlightNgrams: splitAcc': i < j"
          EQ -> L.reverse pref /\ acc
          GT ->
            case acc of
              Nil -> crashWith "highlightNgrams: splitAcc': acc=Nil" -- pref /\ Nil
              elt@(s /\ ls) : elts ->
                let slen = S.length s in
                case compare i (j + slen) of
                  LT -> let {before: s0, after: s1} = S.splitAt (i - j) s in
                        L.reverse ((s0 /\ ls) : pref) /\ ((s1 /\ ls) : elts)
                  EQ -> L.reverse (elt : pref) /\ elts
                  GT -> go (j + slen) (elt : pref) elts


    extractInputTextMatch :: Int -> Int -> String -> String
    extractInputTextMatch i len input = undb $ S.take len $ S.drop (i + 1) input

    addNgramElt ng ne_list (elt /\ elt_lists) = (elt /\ ((ng /\ ne_list) : elt_lists))

    goAcc :: Partial => Int -> HighlightAccumulator -> Tuple NgramsTerm Int -> HighlightAccumulator
    goAcc i acc (pat /\ lpat) =
      case lookupRootList pat table of
        Nothing ->
          crashWith "highlightNgrams: pattern missing from table"
        Just ne_list ->
          let
            (acc0 /\ acc1_2) = splitAcc i acc
            (acc1 /\ acc2) = splitAcc (lpat + 1) acc1_2
            text = extractInputTextMatch i lpat input
            ng = normNgram ntype text
          in
            acc0 <> (addNgramElt ng ne_list <$> acc1) <> acc2

    goFold :: Partial => HighlightAccumulator -> Tuple Int (Array Int) -> HighlightAccumulator
    goFold acc (Tuple i pis) = foldl (goAcc i) acc $
                           --  A.sortWith snd $
                               map (\pat -> pat /\ S.length (db (ngramsTermText pat))) $
                               fromMaybe' (\_ -> crashWith "highlightNgrams: out of bounds pattern") $
                               traverse (A.index pats) pis

-----------------------------------------------------------------------------------

type VersionedNgramsTable = Versioned NgramsTable
type VersionedWithCountNgramsTable = VersionedWithCount NgramsTable

-----------------------------------------------------------------------------------
data Replace a
  = Keep
  | Replace { old :: a, new :: a }

derive instance Generic (Replace a) _

replace :: forall a. Eq a => a -> a -> Replace a
replace old new
  | old == new = Keep
  | otherwise  = Replace { old, new }

derive instance Eq a => Eq (Replace a)

instance Eq a => Semigroup (Replace a) where
  append Keep p = p
  append p Keep = p
  append (Replace { old }) (Replace { new }) | old /= new = unsafeThrow "old != new"
  append (Replace { new }) (Replace { old }) = replace old new

instance Eq a => Monoid (Replace a) where mempty = Keep

applyReplace :: forall a. Eq a => Replace a -> a -> a
applyReplace Keep a = a
applyReplace (Replace { old, new }) a
  | a == old  = new
  | otherwise = a

instance JSON.WriteForeign a => JSON.WriteForeign (Replace a) where
  writeImpl Keep = JSON.writeImpl { tag: "Keep" }
  writeImpl (Replace {old, new}) = JSON.writeImpl { old, new, tag: "Replace" }
instance (JSON.ReadForeign a, Eq a) => JSON.ReadForeign (Replace a) where
  readImpl f = do
    impl :: { old :: Maybe a, new :: Maybe a }  <- JSON.readImpl f
    case Tuple impl.old impl.new of
      Tuple (Just old) (Just new) -> pure $ replace old new
      Tuple Nothing Nothing       -> pure Keep
      _                           -> F.fail $ F.ForeignError "decodeJsonReplace"

-- Representing a PatchSet as `Map a Boolean` would have the advantage
-- of enforcing rem and add to be disjoint.
newtype PatchSet a = PatchSet
  { rem :: Set a
  , add :: Set a
  }

derive instance Generic (PatchSet a) _
derive instance Newtype (PatchSet a) _

instance Ord a => Semigroup (PatchSet a) where
  append (PatchSet p) (PatchSet q) = PatchSet
    { rem: q.rem <> p.rem
    , add: Set.difference q.add p.rem <> p.add
    }

instance Ord a => Monoid (PatchSet a) where
  mempty = PatchSet { rem: Set.empty, add: Set.empty }

instance JSON.WriteForeign a => JSON.WriteForeign (PatchSet a) where
  writeImpl (PatchSet {rem, add}) = JSON.writeImpl { rem: (Set.toUnfoldable rem :: Array a)
                                                   , add: (Set.toUnfoldable add :: Array a) }

instance (Ord a, JSON.ReadForeign a) => JSON.ReadForeign (PatchSet a) where
  readImpl f = do
    -- TODO handle empty fields
    inst :: { rem :: Array a, add :: Array a } <- JSON.readImpl f
    let rem = mkSet inst.rem
        add = mkSet inst.add
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

-- TODO shall we normalise as in replace? shall we make a type class Replaceable?
ngramsReplace :: Maybe NgramsRepoElement -> Maybe NgramsRepoElement -> NgramsPatch
ngramsReplace patch_old patch_new = NgramsReplace {patch_old, patch_new}

derive instance Eq (PatchSet NgramsTerm)

-- TODO
invert :: forall a. a -> a
invert _ = unsafeThrow "invert: TODO"



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


fromMap :: forall k p. Ord k => Eq p => Monoid p => Map k p -> PatchMap k p
fromMap = PatchMap <<< Map.filter (\v -> v /= mempty)

instance (Ord k, Eq p, Monoid p) => Semigroup (PatchMap k p) where
  append (PatchMap p) (PatchMap q) = fromMap $ Map.unionWith append p q

instance (Ord k, Eq p, Monoid p) => Monoid (PatchMap k p) where
  mempty = PatchMap Map.empty

_PatchMap :: forall k p. Iso' (PatchMap k p) (Map k p)
_PatchMap = _Newtype

{-
instance Functor (PatchMap k) where
  map f (PatchMap m) = PatchMap (map f m) -- NO NORM: fromMap would not typecheck

instance FunctorWithIndex k (PatchMap k) where
  mapWithIndex f (PatchMap m) = PatchMap (mapWithIndex f m) -- NO NORM: fromMap would not typecheck
-}

instance Foldable (PatchMap k) where
  foldr f z (PatchMap m) = foldr f z m
  foldl f z (PatchMap m) = foldl f z m
  foldMap f (PatchMap m) = foldMap f m

instance FoldableWithIndex k (PatchMap k) where
  foldrWithIndex f z (PatchMap m) = foldrWithIndex f z m
  foldlWithIndex f z (PatchMap m) = foldlWithIndex f z m
  foldMapWithIndex f (PatchMap m) = foldMapWithIndex f m

{- fromMap is preventing these to type check:

instance Ord k => Traversable (PatchMap k) where
  traverse f (PatchMap m) = fromMap <$> traverse f m
  sequence (PatchMap m) = fromMap <$> sequence m

instance Ord k => TraversableWithIndex k (PatchMap k) where
  traverseWithIndex f (PatchMap m) = fromMap <$> traverseWithIndex f m
-}

traversePatchMapWithIndex :: forall f a b k.
                             Applicative f => Ord k => Eq b => Monoid b =>
                             (k -> a -> f b) -> PatchMap k a -> f (PatchMap k b)
traversePatchMapWithIndex f (PatchMap m) = fromMap <$> traverseWithIndex f m

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
{-
applyPatchMap applyPatchValue (PatchMap pm) m = mergeMap f pm m
  where
    f _ (This pv)   = applyPatchValue pv Nothing
    f _ (That v)    = Just v
    f _ (Both pv v) = applyPatchValue pv (Just v)
-}
applyPatchMap applyPatchValue (PatchMap pm) m =
    foldl go m (Map.toUnfoldable pm :: List (Tuple k p))
  where
    go m' (Tuple k pv) = Map.alter (applyPatchValue pv) k m'

type VersionedNgramsPatches = Versioned NgramsPatches

newtype AsyncNgramsChartsUpdate = AsyncNgramsChartsUpdate {
    listId  :: Maybe ListId
  , tabType :: TabType
  }
derive instance Generic AsyncNgramsChartsUpdate _
derive instance Newtype AsyncNgramsChartsUpdate _
instance JSON.WriteForeign AsyncNgramsChartsUpdate where
  writeImpl (AsyncNgramsChartsUpdate { listId, tabType }) =
    JSON.writeImpl { list_id: listId, tab_type: tabType }

type NewElems = Map NgramsTerm TermList

----------------------------------------------------------------------------------
isEmptyNgramsTablePatch :: NgramsTablePatch -> Boolean
isEmptyNgramsTablePatch {ngramsPatches} = isEmptyPatchMap ngramsPatches

fromNgramsPatches :: NgramsPatches -> NgramsTablePatch
fromNgramsPatches ngramsPatches = {ngramsPatches}

findNgramRoot :: NgramsTable -> NgramsTerm -> NgramsTerm
findNgramRoot (NgramsTable m) n =
  fromMaybe n (m.ngrams_repo_elements ^? at n <<< _Just <<< _NgramsRepoElement <<< _root <<< _Just)

findNgramTermList :: NgramsTable -> NgramsTerm -> Maybe TermList
findNgramTermList (NgramsTable m) n = m.ngrams_repo_elements ^? at r <<< _Just <<< _NgramsRepoElement <<< _list
  where
    r = findNgramRoot (NgramsTable m) n

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
putNgramsPatches { listIds, nodeId, session, tabType } = put session putNgrams
  where putNgrams = PutNgrams tabType (head listIds) Nothing (Just nodeId)

syncPatches :: forall p s. CoreParams p -> T.Box (CoreState s) -> (Unit -> Aff Unit) -> Effect Unit
syncPatches props state callback = do
  { ngramsLocalPatch: ngramsLocalPatch@{ ngramsPatches }
  , ngramsStagePatch
  , ngramsValidPatch
  , ngramsVersion } <- T.read state
  when (isEmptyNgramsTablePatch ngramsStagePatch) $ do
    let pt = Versioned { data: ngramsPatches, version: ngramsVersion }
    launchAff_ $ do
      Versioned { data: newPatch, version: newVersion } <- putNgramsPatches props pt
      callback unit
      liftEffect $ do
        log2 "[syncPatches] setting state, newVersion" newVersion
        T.modify_ (\s ->
          -- I think that sometimes this setState does not fully go through.
          -- This is an issue because the version number does not get updated and the subsequent calls
          -- can mess up the patches.
          s {
              ngramsLocalPatch = fromNgramsPatches mempty
            , ngramsStagePatch = fromNgramsPatches mempty
            , ngramsValidPatch = fromNgramsPatches newPatch <> ngramsLocalPatch <> s.ngramsValidPatch
                              -- First the already valid patch, then the local patch, then the newly received newPatch.
            , ngramsVersion    = newVersion
            }) state
        log2 "[syncPatches] ngramsVersion" newVersion
    pure unit

{-
syncPatchesAsync :: forall p s. CoreParams p -> R.State (CoreState s) -> (Unit -> Aff Unit) -> Effect Unit
syncPatchesAsync props@{ listIds, tabType }
                 ({ ngramsLocalPatch: ngramsLocalPatch@{ ngramsPatches }
                  , ngramsStagePatch
                  , ngramsValidPatch
                  , ngramsVersion
                  } /\ setState) callback = do
  when (isEmptyNgramsTablePatch ngramsStagePatch) $ do
    let patch = Versioned { data: ngramsPatches, version: ngramsVersion }
    launchAff_ $ do
      Versioned { data: newPatch, version: newVersion } <- postNgramsPatchesAsync props patch
      callback unit
      liftEffect $ do
        log2 "[syncPatches] setting state, newVersion" newVersion
        setState $ \s ->
          s {
              ngramsLocalPatch = fromNgramsPatches mempty
            , ngramsStagePatch = fromNgramsPatches mempty
            , ngramsValidPatch = fromNgramsPatches newPatch <> ngramsLocalPatch <> s.ngramsValidPatch
                              -- First the already valid patch, then the local patch, then the newly received newPatch.
            , ngramsVersion    = newVersion
            }
        log2 "[syncPatches] ngramsVersion" newVersion
-}

commitPatch :: forall s. NgramsTablePatch -> T.Box (CoreState s) -> Effect Unit
commitPatch tablePatch state = do
  T.modify_ (\s -> s { ngramsLocalPatch = tablePatch <> s.ngramsLocalPatch }) state
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
  | Synchronize { afterSync  :: Unit -> Aff Unit }
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

coreDispatch :: forall p s. CoreParams p -> T.Box (CoreState s) -> CoreDispatch
coreDispatch path state (Synchronize { afterSync }) =
  syncPatches path state afterSync
coreDispatch _ state (CommitPatch pt) =
  commitPatch pt state
coreDispatch _ state ResetPatches =
  T.modify_ (_ { ngramsLocalPatch = { ngramsPatches: mempty } }) state

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


------------------------------------------------------------------------
-- | Reset Button
type SyncResetButtonsProps =
  ( afterSync        :: Unit -> Aff Unit
  , ngramsLocalPatch :: NgramsTablePatch
  , performAction    :: CoreDispatch
  )

syncResetButtons :: Record SyncResetButtonsProps -> R.Element
syncResetButtons p = R.createElement syncResetButtonsCpt p []

syncResetButtonsCpt :: R.Component SyncResetButtonsProps
syncResetButtonsCpt = here.component "syncResetButtons" cpt
  where
    cpt { afterSync, ngramsLocalPatch, performAction } _ = do
      synchronizing <- T.useBox false
      synchronizing' <- T.useLive T.unequal synchronizing

      let
        hasChanges = ngramsLocalPatch /= mempty
        hasChangesClass = if hasChanges then "" else " disabled"

        synchronizingClass = if synchronizing' then " disabled" else ""

        resetClick _ = do
          performAction ResetPatches

        synchronizeClick _ = delay unit $ \_ -> do
          T.write_ true synchronizing
          performAction $ Synchronize { afterSync: newAfterSync }

        newAfterSync x = do
          afterSync x
          liftEffect $ T.write_ false synchronizing

      pure $ H.div { className: "btn-toolbar" }
        [ H.div { className: "btn-group mr-2" }
          [ H.button { className: "btn btn-danger " <> hasChangesClass <> synchronizingClass
                     , on: { click: resetClick }
                     } [ H.text "Reset" ]
          ]
        , H.div { className: "btn-group mr-2" }
          [ H.button { className: "btn btn-primary " <> hasChangesClass <> synchronizingClass
                     , on: { click: synchronizeClick }
                     } [ H.text "Sync" ]
          ]
        ]


type ResetButton = (Unit -> Aff Unit)
               -> { ngramsPatches :: PatchMap NgramsTerm NgramsPatch }
               -> (Action -> Effect Unit)
               -> Array R.Element

chartsAfterSync :: forall props discard.
  { listIds :: Array Int
  , nodeId  :: Int
  , session :: Session
  , tabType :: TabType
  | props
  }
  -> T.Box GAT.Storage
  -> discard
  -> Aff Unit
chartsAfterSync path'@{ nodeId } tasks _ = do
  task <- postNgramsChartsAsync path'
  liftEffect $ do
    log2 "[chartsAfterSync] Synchronize task" task
    GAT.insert nodeId task tasks

postNgramsChartsAsync :: forall s. CoreParams s -> Aff AsyncTaskWithType
postNgramsChartsAsync { listIds, nodeId, session, tabType } = do
    task <- post session putNgramsAsync acu
    pure $ AsyncTaskWithType { task, typ: UpdateNgramsCharts }
  where
    acu = AsyncNgramsChartsUpdate { listId: head listIds
                                  , tabType }
    putNgramsAsync = PostNgramsChartsAsync (Just nodeId)

