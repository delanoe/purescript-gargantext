module Gargantext.Core.NgramsTable.Types where

import Control.Monad.State (class MonadState, execState)
import Data.Bifunctor (lmap)
import Data.Eq.Generic (genericEq)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldlWithIndex, foldrWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', use, view, (%=), (%~), (.~), (?=), (^?))
import Data.Lens.At (class At, at)
import Data.Lens.Common (_Just)
import Data.Lens.Index (class Index, ix)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.Set (Set)
import Data.Set as Set
import Data.String.Regex (Regex, regex, replace) as R
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Exception.Unsafe (unsafeThrow)
import Foreign as F
import Foreign.Object as FO
import Gargantext.Components.Table.Types as T
import Gargantext.Prelude
import Gargantext.Sessions (Session)
import Gargantext.Types as GT
import Simple.JSON as JSON
import Reactix as R
import Type.Proxy (Proxy(..))

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

---------------------------------------------------
newtype NgramsTablePatch = NgramsTablePatch NgramsPatches
derive instance Generic NgramsTablePatch _
derive instance Newtype NgramsTablePatch _
instance Eq NgramsTablePatch where eq = genericEq
derive newtype instance JSON.ReadForeign NgramsTablePatch
derive newtype instance JSON.WriteForeign NgramsTablePatch
derive newtype instance Semigroup NgramsTablePatch
derive newtype instance Monoid NgramsTablePatch


type NgramsPatches = PatchMap NgramsTerm NgramsPatch



fromMap :: forall k p. Ord k => Eq p => Monoid p => Map k p -> PatchMap k p
fromMap = PatchMap <<< Map.filter (\v -> v /= mempty)



ngramsTermText :: NgramsTerm -> String
ngramsTermText (NormNgramsTerm t) = t



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
instance (Ord k, Eq p, Monoid p) => Semigroup (PatchMap k p) where
  append (PatchMap p) (PatchMap q) = fromMap $ Map.unionWith append p q
instance (Ord k, Eq p, Monoid p) => Monoid (PatchMap k p) where
  mempty = PatchMap Map.empty
instance Foldable (PatchMap k) where
  foldr f z (PatchMap m) = foldr f z m
  foldl f z (PatchMap m) = foldl f z m
  foldMap f (PatchMap m) = foldMap f m
instance FoldableWithIndex k (PatchMap k) where
  foldrWithIndex f z (PatchMap m) = foldrWithIndex f z m
  foldlWithIndex f z (PatchMap m) = foldlWithIndex f z m
  foldMapWithIndex f (PatchMap m) = foldMapWithIndex f m

{-
instance Functor (PatchMap k) where
  map f (PatchMap m) = PatchMap (map f m) -- NO NORM: fromMap would not typecheck

instance FunctorWithIndex k (PatchMap k) where
  mapWithIndex f (PatchMap m) = PatchMap (mapWithIndex f m) -- NO NORM: fromMap would not typecheck
-}

{- fromMap is preventing these to type check:

instance Ord k => Traversable (PatchMap k) where
  traverse f (PatchMap m) = fromMap <$> traverse f m
  sequence (PatchMap m) = fromMap <$> sequence m

instance Ord k => TraversableWithIndex k (PatchMap k) where
  traverseWithIndex f (PatchMap m) = fromMap <$> traverseWithIndex f m
-}

_PatchMap :: forall k p. Iso' (PatchMap k p) (Map k p)
_PatchMap = _Newtype



-- TODO shall we normalise as in replace? shall we make a type class Replaceable?
ngramsReplace :: Maybe NgramsRepoElement -> Maybe NgramsRepoElement -> NgramsPatch
ngramsReplace patch_old patch_new = NgramsReplace {patch_old, patch_new}


applyReplace :: forall a. Eq a => Replace a -> a -> a
applyReplace Keep a = a
applyReplace (Replace { old, new }) a
  | a == old  = new
  | otherwise = a



applyPatchSet :: forall a. Ord a => PatchSet a -> Set a -> Set a
applyPatchSet (PatchSet p) s = Set.difference s p.rem <> p.add



applyNgramsPatch' :: forall row.
                          { patch_children :: PatchSet NgramsTerm
                          , patch_list     :: Replace GT.TermList
                          } ->
                     Endo { list     :: GT.TermList
                          , children :: Set NgramsTerm
                          | row
                          }
applyNgramsPatch' p e =
  e { list     = applyReplace p.patch_list e.list
    , children = applyPatchSet p.patch_children e.children
    }


-- TODO
invert :: forall a. a -> a
invert _ = unsafeThrow "invert: TODO"



data NgramsPatch
  = NgramsReplace
      { patch_old :: Maybe NgramsRepoElement
      , patch_new :: Maybe NgramsRepoElement
      }
  | NgramsPatch
      { patch_children :: PatchSet NgramsTerm
      , patch_list     :: Replace GT.TermList
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
            , patch_list :: Replace GT.TermList } <- JSON.readImpl f
    -- TODO handle empty fields
    -- TODO handle patch_new
    if isJust inst.patch_new || isJust inst.patch_old then
      pure $ NgramsReplace { patch_old: inst.patch_old, patch_new: inst.patch_new }
    else do
      pure $ NgramsPatch { patch_list: inst.patch_list, patch_children: inst.patch_children }

-----------------------------------------------------
newtype NgramsTerm = NormNgramsTerm String
derive instance Generic NgramsTerm _
derive instance Newtype NgramsTerm _
instance Eq NgramsTerm where eq = genericEq
instance Ord NgramsTerm where compare = genericCompare
instance Show NgramsTerm where show = genericShow
derive newtype instance JSON.ReadForeign NgramsTerm
derive newtype instance JSON.WriteForeign NgramsTerm
derive newtype instance Monoid NgramsTerm

--------------------------------------------------------
type CoreParams s =
  { nodeId  :: Int
    -- ^ This node can be a corpus or contact.
  , listIds :: Array Int
  , tabType :: GT.TabType
  , session :: Session
  | s
  }

type PageParams =
  CoreParams
    ( params         :: T.Params
    , searchQuery    :: String
    , termListFilter :: Maybe GT.TermList -- Nothing means all
    , termSizeFilter :: Maybe GT.TermSize -- Nothing means all
    , scoreType      :: GT.ScoreType
    )

-----------------------------------------------------------------------------------
newtype NgramsElement = NgramsElement
  { ngrams      :: NgramsTerm -- HERE
  , size        :: Int -- MISSING
  , list        :: GT.TermList -- ok
  , root        :: Maybe NgramsTerm -- ok
  , parent      :: Maybe NgramsTerm -- ok
  , children    :: Set NgramsTerm -- ok
  , occurrences :: Set Int -- HERE
  }
derive instance Eq NgramsElement
derive instance Newtype NgramsElement _
derive instance Generic NgramsElement _
instance Show NgramsElement where show = genericShow
instance JSON.ReadForeign NgramsElement where
  readImpl f = do
    inst :: { children :: Array NgramsTerm
            , size :: Int
            , list :: GT.TermList
            , ngrams :: NgramsTerm
            , occurrences :: Array Int
            , parent :: Maybe NgramsTerm
            , root :: Maybe NgramsTerm } <- JSON.readImpl f
    pure $ NgramsElement $ inst { children = Set.fromFoldable inst.children
                                , occurrences = Set.fromFoldable inst.occurrences }
instance JSON.WriteForeign NgramsElement where
  writeImpl (NgramsElement ne) =
    JSON.writeImpl $ ne { children = Set.toUnfoldable ne.children :: Array _
                        , occurrences = Set.toUnfoldable ne.occurrences :: Array _ }

_parent :: forall parent row. Lens' { parent :: parent | row } parent
_parent = prop (Proxy :: Proxy "parent")

_root :: forall root row. Lens' { root :: root | row } root
_root   = prop (Proxy :: Proxy "root")

_ngrams :: forall row. Lens' { ngrams :: NgramsTerm | row } NgramsTerm
_ngrams = prop (Proxy :: Proxy "ngrams")

_children :: forall row. Lens' { children :: Set NgramsTerm | row } (Set NgramsTerm)
_children = prop (Proxy :: Proxy "children")

_occurrences :: forall row. Lens' { occurrences :: Set Int | row } (Set Int)
_occurrences = prop (Proxy :: Proxy "occurrences")

_list :: forall a row. Lens' { list :: a | row } a
_list = prop (Proxy :: Proxy "list")

_ngrams_repo_elements :: forall a row. Lens' { ngrams_repo_elements :: a | row } a
_ngrams_repo_elements = prop (Proxy :: Proxy "ngrams_repo_elements")

_ngrams_scores :: forall a row. Lens' { ngrams_scores :: a | row } a
_ngrams_scores = prop (Proxy :: Proxy "ngrams_scores")

_NgramsElement  :: Iso' NgramsElement {
    children    :: Set NgramsTerm
  , size        :: Int
  , list        :: GT.TermList
  , ngrams      :: NgramsTerm
  , occurrences :: Set Int
  , parent      :: Maybe NgramsTerm
  , root        :: Maybe NgramsTerm
  }
_NgramsElement = _Newtype

type NgramsRepoElementT =
  ( size :: Int
  , list     :: GT.TermList
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
  , list        :: GT.TermList
  , parent      :: Maybe NgramsTerm
  , root        :: Maybe NgramsTerm
--  , occurrences :: Int
  }
_NgramsRepoElement = _Newtype

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
  , ngrams_scores        :: Map NgramsTerm (Set Int)
  }
derive instance Newtype NgramsTable _
derive instance Generic NgramsTable _
instance Eq NgramsTable where eq = genericEq
instance Show NgramsTable where show = genericShow
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
      g (NgramsElement e) = Tuple e.ngrams e.occurrences

_NgramsTable :: Iso' NgramsTable
                     { ngrams_repo_elements :: Map NgramsTerm NgramsRepoElement
                     , ngrams_scores        :: Map NgramsTerm (Set Int)
                     }
_NgramsTable = _Newtype

instance Index NgramsTable NgramsTerm NgramsRepoElement where
  ix k = _NgramsTable <<< _ngrams_repo_elements <<< ix k

instance At NgramsTable NgramsTerm NgramsRepoElement where
  at k = _NgramsTable <<< _ngrams_repo_elements <<< at k

{- NOT USED
instance EncodeJson NgramsTable where
  encodeJson (NgramsTable {ngrams_repo_elements, ngrams_scores}) = encodeJson $ Map.values ... TODO
-}
--------------------------------------------

type HighlightElement = Tuple String (List (Tuple NgramsTerm GT.TermList))
type HighlightAccumulator = List HighlightElement

-----------------------------------------------------------------------------------

type VersionedNgramsTable = Versioned NgramsTable
type VersionedWithCountNgramsTable = VersionedWithCount NgramsTable

-----------------------------------------------------------------------------------

replace :: forall a. Eq a => a -> a -> Replace a
replace old new
  | old == new = Keep
  | otherwise  = Replace { old, new }


data Replace a
  = Keep
  | Replace { old :: a, new :: a }
derive instance Generic (Replace a) _
derive instance Eq a => Eq (Replace a)
instance Eq a => Semigroup (Replace a) where
  append Keep p = p
  append p Keep = p
  append (Replace { old }) (Replace { new }) | old /= new = unsafeThrow "old != new"
  append (Replace { new }) (Replace { old }) = replace old new
instance Eq a => Monoid (Replace a) where mempty = Keep
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

---------------------------------------------------

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
derive instance Eq (PatchSet NgramsTerm)
-----------------------------------------------------

type VersionedNgramsPatches = Versioned NgramsPatches

newtype AsyncNgramsChartsUpdate = AsyncNgramsChartsUpdate {
    listId  :: Maybe GT.ListId
  , tabType :: GT.TabType
  }
derive instance Generic AsyncNgramsChartsUpdate _
derive instance Newtype AsyncNgramsChartsUpdate _
instance JSON.WriteForeign AsyncNgramsChartsUpdate where
  writeImpl (AsyncNgramsChartsUpdate { listId, tabType }) =
    JSON.writeImpl { list_id: listId, tab_type: tabType }

type NewElems = Map NgramsTerm GT.TermList

type RootParent = { root :: NgramsTerm, parent :: NgramsTerm }

type ReParent a = forall m. MonadState NgramsTable m => a -> m Unit
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

type NgramsListByTabType = Map GT.TabType VersionedNgramsTable

data CoreAction
  = CommitPatch NgramsTablePatch
  | Synchronize { afterSync  :: Unit -> Aff Unit }
  | ResetPatches

data Action
  = CoreAction CoreAction
  | ClearTreeEdit
  | SetParentResetChildren (Maybe NgramsTerm) (List NgramsTerm)
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



type NgramsDepth = { ngrams :: NgramsTerm, depth :: Int }
type NgramsClick = NgramsDepth -> Maybe (Effect Unit)
type NgramsActionRef = R.Ref (Maybe (Unit -> Effect Unit))


type ResetButton = (Unit -> Aff Unit)
               -> NgramsTablePatch
               -> (Action -> Effect Unit)
               -> Array R.Element
