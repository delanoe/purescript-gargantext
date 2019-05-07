module Gargantext.Components.NgramsTable.Spec where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Gargantext.Components.NgramsTable (highlightNgrams, NgramsElement(..), NgramsTable(..))
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
          output = [Tuple "this is a" Nothing
                   ,Tuple " " Nothing
                   ,Tuple "graph" (Just GraphTerm)
                   ,Tuple " " Nothing
                   ,Tuple "about a biography" Nothing
                   ,Tuple " " Nothing
                   ,Tuple "which" (Just StopTerm)
                   ,Tuple " " Nothing
                   ,Tuple " " Nothing
                   ,Tuple "stops" (Just StopTerm)
                   ,Tuple " " Nothing
                   ,Tuple "at every" Nothing
                   ,Tuple " " Nothing
                   ,Tuple "candidate" (Just CandidateTerm)
                   ,Tuple " " Nothing
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
          output = [Tuple "This" Nothing
                   ,Tuple " " Nothing
                   ,Tuple "is" (Just StopTerm)
                   ,Tuple " " Nothing
                   ,Tuple " " Nothing
                   ,Tuple "a" (Just StopTerm)
                   ,Tuple " " Nothing
                   ,Tuple " " Nothing
                   ,Tuple "new" (Just GraphTerm)
                   ,Tuple " " Nothing
                   ,Tuple " " Nothing
                   ,Tuple "state" (Just GraphTerm)
                   ,Tuple " " Nothing
                   ,Tuple " " Nothing
                   ,Tuple "of" (Just StopTerm)
                   ,Tuple " " Nothing
                   ,Tuple " " Nothing
                   ,Tuple "the" (Just GraphTerm)
                   ,Tuple " " Nothing
                   ]
      highlightNgrams table input `shouldEqual` output

    it "works when pattern overlaps 2" do
      let table = NgramsTable
                    (Map.fromFoldable [tne "from"   GraphTerm
                                      ,tne "i"      StopTerm
                                      ,tne "images" GraphTerm
                                      ])
          input = "This is from space images"
          output = [Tuple "This is" Nothing
                   ,Tuple " " Nothing
                   ,Tuple "from" (Just GraphTerm)
                   ,Tuple " " Nothing
                   ,Tuple "space" Nothing
                   ,Tuple " " Nothing
                   ,Tuple "images" (Just GraphTerm)
                   ,Tuple " " Nothing
                   ]
      highlightNgrams table input `shouldEqual` output
