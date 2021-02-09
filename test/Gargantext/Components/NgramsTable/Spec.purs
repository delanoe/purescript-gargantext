module Gargantext.Components.NgramsTable.Spec where

import Prelude
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Map as Map
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
-- import Test.Spec.Assertions (shouldEqual)
-- import Test.Spec.QuickCheck (quickCheck')

import Test.Utils (shouldEqualArray)

import Gargantext.Components.NgramsTable.Core (highlightNgrams, HighlightElement, NgramsElement(..), NgramsRepoElement(..), NgramsTable(..), NgramsTerm, normNgram)
import Gargantext.Types (CTabNgramType(..), TermList(..))


ne :: String -> TermList -> CTabNgramType -> NgramsElement
ne ngrams list ngramType = NgramsElement { ngrams: normed
                                         , size: 1  -- TODO
                                         , list
                                         , occurrences: 0
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
      let table = NgramsTable
                   { ngrams_repo_elements: Map.fromFoldable [ tnre "which"     StopTerm ngramType
                                                            , tnre "stops"     StopTerm ngramType
                                                            , tnre "candidate" CandidateTerm ngramType
                                                            ]
                   , ngrams_scores: Map.fromFoldable [] }
          input = "this is a graph about a biography which stops at every candidate"
          output = [ highlightNil " this is a graph about a biography "
                   , highlightSingleton " which" ngramType StopTerm
                   , highlightNil " "
                   , highlightSingleton " stops" ngramType StopTerm
                   , highlightNil " at every "
                   , highlightSingleton " candidate" ngramType CandidateTerm
                   , highlightNil " "
                   ]
      highlightNgrams CTabTerms table input `shouldEqualArray` output

    it "works when pattern overlaps" do
      let ngramType = CTabSources
      let table = NgramsTable
                    { ngrams_repo_elements: Map.fromFoldable [ tnre "is"     StopTerm ngramType
                                                             , tnre "a"      StopTerm ngramType
                                                             , tnre "of"     StopTerm ngramType
                                                             ]
                    , ngrams_scores: Map.fromFoldable [] }
          input = "This is a new state of the"
          output = [ highlightNil " This "
                   , highlightSingleton " is" ngramType StopTerm
                   , highlightNil " "
                   , highlightSingleton " a" ngramType StopTerm
                   , highlightNil " new state "
                   , highlightSingleton " of" ngramType StopTerm
                   , highlightNil " the "
                   ]
      highlightNgrams CTabTerms table input `shouldEqualArray` output

    it "works when pattern overlaps 2" do
      let ngramType = CTabSources
      let table = NgramsTable
                    { ngrams_repo_elements: Map.fromFoldable [ tnre "from"   CandidateTerm ngramType
                                                             , tnre "i"      StopTerm ngramType
                                                             , tnre "images" CandidateTerm ngramType
                                                             ]
                    , ngrams_scores: Map.fromFoldable [] }
          input = "This is from space images"
          output = [ highlightNil " This is "
                   , highlightSingleton " from" ngramType CandidateTerm
                   , highlightNil " space "
                   , highlightSingleton " images" ngramType CandidateTerm
                   , highlightNil " "
                   ]
      highlightNgrams CTabTerms table input `shouldEqualArray` output

    it "works when pattern overlaps 3" do
      let ngramType = CTabSources
      let table = NgramsTable
                    { ngrams_repo_elements: Map.fromFoldable [ tnre "something"             CandidateTerm ngramType
                                                             , tnre "something different"   MapTerm ngramType
                                                             ]
                    , ngrams_scores: Map.fromFoldable [] }
          input = "and now for something different"
          output = [ highlightNil " and now for "
                   , Tuple " something" $ L.fromFoldable [
                         highlightTuple "something different" ngramType MapTerm
                       , highlightTuple "something" ngramType CandidateTerm
                       ]
                   , Tuple " different" $ L.singleton $ highlightTuple "something different" ngramType MapTerm
                   , highlightNil " "
                   ]
      highlightNgrams CTabTerms table input `shouldEqualArray` output

    it "works with punctuation" do
      let ngramType = CTabSources
      let table = NgramsTable
                    { ngrams_repo_elements: Map.fromFoldable [ tnre "graph" CandidateTerm ngramType ]
                    , ngrams_scores: Map.fromFoldable [] }
          input = "before graph, after"
          output = [ highlightNil " before "
                   , highlightSingleton " graph" ngramType CandidateTerm
                   , highlightNil ", after "
                   ]
      highlightNgrams CTabTerms table input `shouldEqualArray` output
