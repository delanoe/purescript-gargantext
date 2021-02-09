module Gargantext.Components.NgramsTable.Spec where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Map as Map
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
-- import Test.Spec.QuickCheck (quickCheck')

import Gargantext.Components.NgramsTable.Core (highlightNgrams, NgramsElement(..), NgramsRepoElement(..), NgramsTable(..), NgramsTerm, normNgram)
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
          output = [ Tuple "this is a graph about a biography " Nothing
                   , Tuple "which" (Just StopTerm)
                   , Tuple " " Nothing
                   , Tuple "stops" (Just StopTerm)
                   , Tuple " at every " Nothing
                   , Tuple "candidate" (Just CandidateTerm)
                   ]
      highlightNgrams CTabTerms table input `shouldEqual` output

    it "works when pattern overlaps" do
      let ngramType = CTabSources
      let table = NgramsTable
                    { ngrams_repo_elements: Map.fromFoldable [ tnre "is"     StopTerm ngramType
                                                             , tnre "a"      StopTerm ngramType
                                                             , tnre "of"     StopTerm ngramType
                                                             ]
                    , ngrams_scores: Map.fromFoldable [] }
          input = "This is a new state of the"
          output = [ Tuple "This " Nothing
                   , Tuple "is" (Just StopTerm)
                   , Tuple " " Nothing
                   , Tuple "a" (Just StopTerm)
                   , Tuple " new state " Nothing
                   , Tuple "of" (Just StopTerm)
                   , Tuple " the" Nothing
                   ]
      highlightNgrams CTabTerms table input `shouldEqual` output

    it "works when pattern overlaps 2" do
      let ngramType = CTabSources
      let table = NgramsTable
                    { ngrams_repo_elements: Map.fromFoldable [ tnre "from"   CandidateTerm ngramType
                                                             , tnre "i"      StopTerm ngramType
                                                             , tnre "images" CandidateTerm ngramType
                                                             ]
                    , ngrams_scores: Map.fromFoldable [] }
          input = "This is from space images"
          output = [ Tuple "This is " Nothing
                   , Tuple "from" (Just CandidateTerm)
                   , Tuple " space " Nothing
                   , Tuple "images" (Just CandidateTerm)
                   ]
      highlightNgrams CTabTerms table input `shouldEqual` output

    it "works when pattern overlaps 3" do
      let ngramType = CTabSources
      let table = NgramsTable
                    { ngrams_repo_elements: Map.fromFoldable [ tnre "fusion"             MapTerm ngramType
                                                             , tnre "calculate fusion"   CandidateTerm ngramType
                                                             ]
                    , ngrams_scores: Map.fromFoldable [] }
          input = "Model has been used to calculate fusion cross sections"
          output = [ Tuple "Model has been used to " Nothing
                   , Tuple "calculate fusion" (Just CandidateTerm)
                   , Tuple " sections " Nothing
                   ]
      highlightNgrams CTabTerms table input `shouldEqual` output

    it "works with punctuation" do
      let ngramType = CTabSources
      let table = NgramsTable
                    { ngrams_repo_elements: Map.fromFoldable [ tnre "graph" CandidateTerm ngramType ]
                    , ngrams_scores: Map.fromFoldable [] }
          input = "before graph, after"
          output = [ Tuple "before " Nothing
                   , Tuple "graph" (Just CandidateTerm)
                   , Tuple ", after" Nothing
                   ]
      highlightNgrams CTabTerms table input `shouldEqual` output
