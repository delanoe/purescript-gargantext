module Test.Gargantext.Components.NgramsTable.Spec where

-- import Test.Spec.QuickCheck (quickCheck')
import Data.List as L
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst)
import Gargantext.Core.NgramsTable.Functions (highlightNgrams, normNgram, computeCache, lookupRootListWithChildren)
import Gargantext.Core.NgramsTable.Types (HighlightElement, NgramsElement(..), NgramsRepoElement(..), NgramsTable(..), NgramsTerm)
import Gargantext.Types (CTabNgramType(..), TermList(..))
import Prelude
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldContain, shouldNotContain, shouldSatisfy)
import Test.Utils (shouldEqualArray)


ne :: String -> TermList -> CTabNgramType -> NgramsElement
ne ngrams list ngramType = NgramsElement { ngrams: normed
                                         , size: 1  -- TODO
                                         , list
                                         , occurrences: Set.singleton 0
                                         , parent:   Nothing
                                         , root:     Nothing
                                         , children: Set.empty
                                         }
  where
    normed = normNgram ngramType ngrams

tne :: String -> TermList -> CTabNgramType -> Tuple NgramsTerm NgramsElement
tne ngrams list ngramType = Tuple normed (ne ngrams list ngramType)
  where
    normed = normNgram ngramType ngrams

nre :: String -> TermList -> CTabNgramType -> NgramsRepoElement
nre ngrams list ngramType = NgramsRepoElement { size: 1  -- TODO
                                              , list
                                              , parent:   Nothing
                                              , root:     Nothing
                                              , children: Set.empty
                                              }

tnre :: String -> TermList -> CTabNgramType -> Tuple NgramsTerm NgramsRepoElement
tnre ngrams list ngramType = Tuple normed (nre ngrams list ngramType)
  where
    normed = normNgram ngramType ngrams

highlightNil :: String -> HighlightElement
highlightNil s = Tuple s L.Nil

highlightTuple :: String -> CTabNgramType -> TermList -> Tuple NgramsTerm TermList
highlightTuple s ngramType term = Tuple (normNgram ngramType s) term

highlightSingleton :: String -> CTabNgramType -> TermList -> HighlightElement
highlightSingleton s ngramType term = Tuple s (L.singleton $ highlightTuple s ngramType term)

spec :: Spec Unit
spec = do
  describe "NgramsTable.highlightNgrams" do
    it "works on a simple example" do
      let ngramType = CTabSources
      let tnres = [ tnre "which"     StopTerm ngramType
                  , tnre "stops"     StopTerm ngramType
                  , tnre "candidate" CandidateTerm ngramType
                  ]
      let table = NgramsTable
                   { ngrams_repo_elements: Map.fromFoldable tnres
                   , ngrams_scores: Map.fromFoldable [] }
          input = "this is a graph about a biography which stops at every candidate"
          cache = computeCache table $ Set.fromFoldable $ fst <$> tnres
          output = [ highlightNil " this is a graph about a biography "
                   , highlightSingleton " which" ngramType StopTerm
                   , highlightNil " "
                   , highlightSingleton " stops" ngramType StopTerm
                   , highlightNil " at every "
                   , highlightSingleton " candidate" ngramType CandidateTerm
                   , highlightNil " "
                   ]
      highlightNgrams cache CTabTerms table input `shouldEqualArray` output

    it "works when pattern overlaps" do
      let ngramType = CTabSources
      let tnres = [ tnre "is"     StopTerm ngramType
                  , tnre "a"      StopTerm ngramType
                  , tnre "of"     StopTerm ngramType
                  ]
      let table = NgramsTable
                    { ngrams_repo_elements: Map.fromFoldable tnres
                    , ngrams_scores: Map.fromFoldable [] }
          input = "This is a new state of the"
          cache = computeCache table $ Set.fromFoldable $ fst <$> tnres
          output = [ highlightNil " This "
                   , highlightSingleton " is" ngramType StopTerm
                   , highlightNil " "
                   , highlightSingleton " a" ngramType StopTerm
                   , highlightNil " new state "
                   , highlightSingleton " of" ngramType StopTerm
                   , highlightNil " the "
                   ]
      highlightNgrams cache CTabTerms table input `shouldEqualArray` output

    it "works when pattern overlaps 2" do
      let ngramType = CTabSources
      let tnres = [ tnre "from"   CandidateTerm ngramType
                  , tnre "i"      StopTerm ngramType
                  , tnre "images" CandidateTerm ngramType
                  ]
      let table = NgramsTable
                    { ngrams_repo_elements: Map.fromFoldable tnres
                    , ngrams_scores: Map.fromFoldable [] }
          input = "This is from space images"
          cache = computeCache table $ Set.fromFoldable $ fst <$> tnres
          output = [ highlightNil " This is "
                   , highlightSingleton " from" ngramType CandidateTerm
                   , highlightNil " space "
                   , highlightSingleton " images" ngramType CandidateTerm
                   , highlightNil " "
                   ]
      highlightNgrams cache CTabTerms table input `shouldEqualArray` output

    it "works when pattern overlaps 3" do
      let ngramType = CTabSources
      let tnres = [ tnre "something"             CandidateTerm ngramType
                  , tnre "something different"   MapTerm ngramType
                  ]
      let table = NgramsTable
                    { ngrams_repo_elements: Map.fromFoldable tnres
                    , ngrams_scores: Map.fromFoldable [] }
          input = "and now for something different"
          cache = computeCache table $ Set.fromFoldable $ fst <$> tnres
          output = [ highlightNil " and now for "
                   , Tuple " something" $ L.fromFoldable [
                         highlightTuple "something different" ngramType MapTerm
                       , highlightTuple "something" ngramType CandidateTerm
                       ]
                   , Tuple " different" $ L.singleton $ highlightTuple "something different" ngramType MapTerm
                   , highlightNil " "
                   ]
      highlightNgrams cache CTabTerms table input `shouldEqualArray` output

    it "works with punctuation" do
      let ngramType = CTabSources
      let tnres = [ tnre "graph" CandidateTerm ngramType ]
      let table = NgramsTable
                    { ngrams_repo_elements: Map.fromFoldable tnres
                    , ngrams_scores: Map.fromFoldable [] }
          input = "before graph, after"
          cache = computeCache table $ Set.fromFoldable $ fst <$> tnres
          output = [ highlightNil " before "
                   , highlightSingleton " graph" ngramType CandidateTerm
                   , highlightNil ", after "
                   ]
      highlightNgrams cache CTabTerms table input `shouldEqualArray` output

    it "works for #598" do
      let input = "Passant d’une conception quasi solipsiste ou monadique de la sensation, présente dans l’Essai sur les données immédiates de la conscience, à l’idée d’une sensibilité universelle mais impersonnelle dans Matière et mémoire, Bergson se heurte à plusieurs difficultés, dont celle d’une dissolution de la notion de Sujet. Dans L’évolution créatrice puis dans Les deux sources de la morale et de la religion, son approche évolutionniste lui permet d’envisager la perspective d’une sensibilité indéfiniment extensible, inséparable désormais de celle d’un Vivant par définition métastable qui va de l’amibe jusqu’à Dieu."
      let startInput = " Passant d’une "
      let middleInput = " quasi solipsiste ou monadique de la sensation, présente dans l’Essai sur les données immédiates de la conscience, à l’idée d’une sensibilité universelle mais impersonnelle dans Matière et mémoire, Bergson se heurte à plusieurs difficultés, dont celle d’une dissolution de la "
      let endInput = " de Sujet. Dans L’évolution créatrice puis dans Les deux sources de la morale et de la religion, son approche évolutionniste lui permet d’envisager la perspective d’une sensibilité indéfiniment extensible, inséparable désormais de celle d’un Vivant par définition métastable qui va de l’amibe jusqu’à Dieu. "
      let ngramType = CTabSources
      -- let termsS = ["arbre","article","automate","automates","calcul","cas","classe","concept","conception","construction","difficultés","données","décidabilité","effet","ensemble","extension","familles","fois","graphes","généralisons","intérêt","langage","langages","logique","longueur inférieure","monadique","mots","notion","opérations","pensée","perception","perspective","point","premier ordre","problème","produit","produits","rendre compte","second ordre","structure","structures","thèse","théorie","théorème","travail","travaux","travers","univers","variétés","vue","étude","évaluation"]
      -- let contextNgramsS = ["concept","conception","difficultés","données","extension","monadique","Nothing"
      --                      ,"notion","perspective"]
      let termsS = ["conception", "notion"]
      let contextNgramsS = ["conception", "notion"]
      let tnres = (\t -> tnre t MapTerm ngramType) <$> termsS
      let table = NgramsTable { ngrams_repo_elements: Map.fromFoldable tnres
                              , ngrams_scores: Map.fromFoldable [] }
      let cache = computeCache table $ Set.fromFoldable $ normNgram ngramType <$> contextNgramsS

      lookupRootListWithChildren (normNgram ngramType "conception") table cache `shouldSatisfy` isJust
      lookupRootListWithChildren (normNgram ngramType "notion") table cache `shouldSatisfy` isJust

      highlightNgrams cache CTabTerms table input `shouldEqualArray`
        [ highlightNil startInput
        , highlightSingleton " conception" ngramType MapTerm
        , highlightNil middleInput
        , highlightSingleton " notion" ngramType MapTerm
        , highlightNil endInput ]
