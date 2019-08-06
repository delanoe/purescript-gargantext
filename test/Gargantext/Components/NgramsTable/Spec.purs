module Gargantext.Components.NgramsTable.Spec where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Gargantext.Components.NgramsTable.Core (highlightNgrams, NgramsElement(..), NgramsTable(..))
import Gargantext.Types  (TermList(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
-- import Test.Spec.QuickCheck (quickCheck')
import Data.Map as Map
import Data.Set as Set

spec :: Spec Unit
spec = do
  let ne ngrams list =
        NgramsElement
          { ngrams
          , list
          , occurrences: 0
          , parent:   Nothing
          , root:     Nothing
          , children: Set.empty
          }
      tne ngrams list = Tuple ngrams (ne ngrams list)
  describe "NgramsTable.highlightNgrams" do
    it "works on a simple example" do
      let table = NgramsTable
                    (Map.fromFoldable [tne "graph"     GraphTerm
                                      ,tne "which"     StopTerm
                                      ,tne "stops"     StopTerm
                                      ,tne "candidate" CandidateTerm
                                      ])
          input = "this is a graph about a biography which stops at every candidate"
          output = [Tuple "this is a " Nothing
                   ,Tuple "graph" (Just GraphTerm)
                   ,Tuple " about a biography " Nothing
                   ,Tuple "which" (Just StopTerm)
                   ,Tuple " " Nothing
                   ,Tuple "stops" (Just StopTerm)
                   ,Tuple " at every " Nothing
                   ,Tuple "candidate" (Just CandidateTerm)
                   ]
      highlightNgrams table input `shouldEqual` output

    it "works when pattern overlaps" do
      let table = NgramsTable
                    (Map.fromFoldable [tne "is"     StopTerm
                                      ,tne "a"      StopTerm
                                      ,tne "of"     StopTerm
                                      ,tne "new"    GraphTerm
                                      ,tne "the"    GraphTerm
                                      ,tne "state"  GraphTerm
                                      ])
          input = "This is a new state of the"
          output = [Tuple "This " Nothing
                   ,Tuple "is" (Just StopTerm)
                   ,Tuple " " Nothing
                   ,Tuple "a" (Just StopTerm)
                   ,Tuple " " Nothing
                   ,Tuple "new" (Just GraphTerm)
                   ,Tuple " " Nothing
                   ,Tuple "state" (Just GraphTerm)
                   ,Tuple " " Nothing
                   ,Tuple "of" (Just StopTerm)
                   ,Tuple " " Nothing
                   ,Tuple "the" (Just GraphTerm)
                   ]
      highlightNgrams table input `shouldEqual` output

    it "works when pattern overlaps 2" do
      let table = NgramsTable
                    (Map.fromFoldable [tne "from"   GraphTerm
                                      ,tne "i"      StopTerm
                                      ,tne "images" GraphTerm
                                      ])
          input = "This is from space images"
          output = [Tuple "This is " Nothing
                   ,Tuple "from" (Just GraphTerm)
                   ,Tuple " space " Nothing
                   ,Tuple "images" (Just GraphTerm)
                   ]
      highlightNgrams table input `shouldEqual` output

    it "works with punctuation" do
      let table = NgramsTable
                    (Map.fromFoldable [tne "graph" GraphTerm])
          input = "before graph, after"
          output = [Tuple "before " Nothing
                   ,Tuple "graph" (Just GraphTerm)
                   ,Tuple ", after" Nothing
                   ]
      highlightNgrams table input `shouldEqual` output
