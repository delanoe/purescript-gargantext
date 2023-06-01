module Gargantext.Core.NgramsTable.Functions
  where

import Gargantext.Prelude

import Control.Monad.State (class MonadState, execState)
import Data.Array (head)
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Lens (use, view, (^?), (^.), (?=), (%~), (%=), (.~))
import Data.Lens.At (at)
import Data.Lens.Common (_Just)
import Data.Lens.Fold (folded, traverseOf_)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.List ((:), List(Nil))
import Data.List as L
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, fromMaybe', isJust)
import Data.Set (Set)
import Data.String as S
import Data.String.Common as DSC
import Data.String.Regex (Regex, regex, replace) as R
import Data.String.Regex.Flags (global, multiline) as R
import Data.String.Search.KarpRabin as SSKR
import Data.String.Utils as SU
import Data.These (These(..))
import Data.Traversable (for, traverse_, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Gargantext.AsyncTasks as GAT
import Gargantext.Components.Table as T
import Gargantext.Components.Table.Types as T
import Gargantext.Config.REST (AffRESTError, RESTError, logRESTError)
import Gargantext.Config.Utils (handleRESTError)
import Gargantext.Core.NgramsTable.Types
import Gargantext.Routes (SessionRoute(..))
import Gargantext.Sessions (Session, get, post, put)
import Gargantext.Types (AsyncTask, AsyncTaskType(..), AsyncTaskWithType(..), CTabNgramType(..), FrontendError, OrderBy(..), ScoreType(..), TabSubType(..), TabType(..), TermList(..), TermSize(..))
import Gargantext.Utils.Either (eitherMap)
--import Gargantext.Utils.KarpRabin (indicesOfAny)
import Gargantext.Utils.Reactix as R2
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)
import Reactix as R
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Core.NgramsTable.Functions"

toVersioned :: forall a. VersionedWithCount a -> Tuple Count (Versioned a)
toVersioned (VersionedWithCount { count, data: d, version }) = Tuple count $ Versioned { data: d, version }

------------------------------------------------------------------------

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

ngramsRepoElementToNgramsElement :: NgramsTerm -> Set Int -> NgramsRepoElement -> NgramsElement
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
highlightNgrams :: CTabNgramType -> NgramsTable -> String -> Array HighlightElement
highlightNgrams ntype table@(NgramsTable {ngrams_repo_elements: elts}) input0 =
    -- trace {pats, input0, input, ixs} \_ ->
    A.fromFoldable ((\(s /\ ls)-> undb s /\ ls) <$> unsafePartial (foldl goFold ((input /\ Nil) : Nil) ixs))
  where
    spR x = " " <> R.replace wordBoundaryReg "$1$1" x <> " "
    -- reR = R.replace wordBoundaryReg " "
    db = S.replaceAll (S.Pattern " ") (S.Replacement "  ")
    sp x = " " <> db x <> " "
    undb = R.replace wordBoundaryReg2 "$1"
    input = spR input0
    pats = A.fromFoldable (Map.keys elts)
    hashStruct = SSKR.hashStruct (sp <<< ngramsTermText <$> pats)
    ixs = SSKR.indicesOfAnyHashStruct hashStruct (normNgramInternal ntype input)

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

--applyNgramsTablePatchToSingleTerm :: NgramsTerm -> NgramsTablePatch -> Set NgramsTerm -> Set NgramsTerm
--applyNgramsTablePatchToSingleTerm ngram patch s =
--  applyNgramsTablePatch patch $

patchSetFromMap :: forall a. Ord a => Map a Boolean -> PatchSet a
patchSetFromMap m = PatchSet { rem: Map.keys (Map.filter not m)
                             , add: Map.keys (Map.filter identity m) }
  -- TODO Map.partition would be nice here



applyNgramsPatch :: NgramsPatch -> Maybe NgramsRepoElement -> Maybe NgramsRepoElement
applyNgramsPatch (NgramsReplace {patch_new}) _ = patch_new
applyNgramsPatch (NgramsPatch p)           m = m # _Just <<< _Newtype %~ applyNgramsPatch' p


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

----------------------------------------------------------------------------------
isEmptyNgramsTablePatch :: NgramsTablePatch -> Boolean
isEmptyNgramsTablePatch (NgramsTablePatch ngramsPatches) = isEmptyPatchMap ngramsPatches

fromNgramsPatches :: NgramsPatches -> NgramsTablePatch
fromNgramsPatches ngramsPatches = NgramsTablePatch ngramsPatches

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
applyNgramsTablePatch (NgramsTablePatch ngramsPatches) (NgramsTable m) =
  execState (reParentNgramsTablePatch ngramsPatches) $
  NgramsTable $ m { ngrams_repo_elements =
                      applyPatchMap applyNgramsPatch ngramsPatches m.ngrams_repo_elements }

applyNgramsPatches :: forall s. CoreState s -> NgramsTable -> NgramsTable
applyNgramsPatches {ngramsLocalPatch, ngramsStagePatch, ngramsValidPatch} =
  applyNgramsTablePatch (ngramsLocalPatch <> ngramsStagePatch <> ngramsValidPatch)
  -- First the valid patch, then the stage patch, and finally the local patch.

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
  NgramsTablePatch $ singletonPatchMap ngrams (newNgramPatch list)

addNewNgramA :: NgramsTerm -> TermList -> CoreAction
addNewNgramA ngrams list = CommitPatch $ addNewNgramP ngrams list

setTermListP :: NgramsTerm -> Replace TermList -> NgramsTablePatch
setTermListP ngram patch_list = singletonNgramsTablePatch ngram pe
  where
    pe = NgramsPatch { patch_list, patch_children: mempty }

setTermListA :: NgramsTerm -> Replace TermList -> CoreAction
setTermListA ngram termList = CommitPatch $ setTermListP ngram termList

putNgramsPatches :: forall s. CoreParams s -> VersionedNgramsPatches -> AffRESTError VersionedNgramsPatches
putNgramsPatches { listIds, nodeId, session, tabType } = put session putNgrams
  where putNgrams = PutNgrams tabType (head listIds) Nothing (Just nodeId)

syncPatches :: forall p s. CoreParams p -> T.Box (CoreState s) -> (Unit -> Aff Unit) -> Effect Unit
syncPatches props state callback = do
  { ngramsLocalPatch: ngramsLocalPatch@(NgramsTablePatch ngramsPatches)
  , ngramsStagePatch
  , ngramsVersion } <- T.read state
  when (isEmptyNgramsTablePatch ngramsStagePatch) $ do
    let pt = Versioned { data: ngramsPatches, version: ngramsVersion }
    launchAff_ $ do
      ePatches <- putNgramsPatches props pt
      case ePatches of
        Left err -> liftEffect $ logRESTError here "[syncPatches]" err
        Right (Versioned { data: newPatch, version: newVersion }) -> do
          callback unit
          liftEffect $ do
            here.log2 "[syncPatches] setting state, newVersion" newVersion
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
            here.log2 "[syncPatches] ngramsVersion" newVersion
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
        here.log2 "[syncPatches] setting state, newVersion" newVersion
        setState $ \s ->
          s {
              ngramsLocalPatch = fromNgramsPatches mempty
            , ngramsStagePatch = fromNgramsPatches mempty
            , ngramsValidPatch = fromNgramsPatches newPatch <> ngramsLocalPatch <> s.ngramsValidPatch
                              -- First the already valid patch, then the local patch, then the newly received newPatch.
            , ngramsVersion    = newVersion
            }
        here.log2 "[syncPatches] ngramsVersion" newVersion
-}

commitPatch :: forall s. NgramsTablePatch -> T.Box (CoreState s) -> Effect Unit
commitPatch tablePatch state = do
  T.modify_ (\s -> s { ngramsLocalPatch = tablePatch <> s.ngramsLocalPatch }) state
    -- First we apply the patches we have locally and then the new patch (tablePatch).

loadNgramsTable :: PageParams -> AffRESTError VersionedNgramsTable
loadNgramsTable
  { nodeId
  , listIds
  , session
  , tabType }
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

loadNgramsTableAll :: PageParams -> AffRESTError NgramsListByTabType
loadNgramsTableAll { nodeId, listIds, session } = do
  let
    cTagNgramTypes =
      [ CTabTerms
      , CTabSources
      , CTabAuthors
      , CTabInstitutes
      ]
    query tabType = GetNgramsTableAll { listIds, tabType } (Just nodeId)

  ret <- Map.fromFoldable <$> for cTagNgramTypes \cTagNgramType -> do
    let tabType = TabCorpus $ TabNgramType cTagNgramType
    result :: Either RESTError VersionedNgramsTable <- get session $ query tabType
    pure $ Tuple tabType result

  pure $ eitherMap ret

convOrderBy :: T.OrderByDirection T.ColumnName -> OrderBy
convOrderBy (T.ASC  (T.ColumnName "Score")) = ScoreAsc
convOrderBy (T.DESC (T.ColumnName "Score")) = ScoreDesc
convOrderBy (T.ASC  _) = TermAsc
convOrderBy (T.DESC _) = TermDesc

coreDispatch :: forall p s. CoreParams p -> T.Box State -> CoreDispatch
coreDispatch path state (Synchronize { afterSync }) =
  syncPatches path state afterSync
coreDispatch _ state (CommitPatch pt) =
  commitPatch pt state
coreDispatch _ state ResetPatches =
  T.modify_ (_ { ngramsLocalPatch = mempty :: NgramsTablePatch
               , ngramsSelection  = mempty :: Set NgramsTerm
               }) state

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


chartsAfterSync :: forall props discard.
  { listIds :: Array Int
  , nodeId  :: Int
  , session :: Session
  , tabType :: TabType
  | props
  }
  -> T.Box (Array FrontendError)
  -> T.Box GAT.Storage
  -> discard
  -> Aff Unit
chartsAfterSync path'@{ nodeId } errors tasks _ = do
  eTask <- postNgramsChartsAsync path'
  handleRESTError here errors eTask $ \task -> liftEffect $ do
    here.log2 "[chartsAfterSync] Synchronize task" task
    GAT.insert nodeId task tasks

postNgramsChartsAsync :: forall s. CoreParams s -> AffRESTError AsyncTaskWithType
postNgramsChartsAsync { listIds, nodeId, session, tabType } = do
    eTask :: Either RESTError AsyncTask <- post session putNgramsAsync acu
    pure $ (\task -> AsyncTaskWithType { task, typ: UpdateNgramsCharts }) <$> eTask
  where
    acu = AsyncNgramsChartsUpdate { listId: head listIds
                                  , tabType }
    putNgramsAsync = PostNgramsChartsAsync (Just nodeId)


tablePatchHasNgrams :: NgramsTablePatch -> NgramsTerm -> Boolean
tablePatchHasNgrams (NgramsTablePatch ngramsPatches) ngrams =
  isJust $ ngramsPatches ^. _PatchMap <<< at ngrams
