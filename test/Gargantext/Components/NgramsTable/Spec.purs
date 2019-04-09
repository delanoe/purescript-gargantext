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
spec =
  describe "NgramsTable.highlightNgrams" do
    it "partially works" do
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
          table = NgramsTable
                    (Map.fromFoldable [tne "graph"     GraphTerm
                                      ,tne "stop"      StopTerm
                                      ,tne "candidate" CandidateTerm
                                      ])
          input = "this is a biography which stops at every candidate"
          output = [Tuple "this is a bio" Nothing
                   ,Tuple "graph" (Just GraphTerm)
                   ,Tuple "y which " Nothing
                   ,Tuple "stop" (Just StopTerm)
                   ,Tuple "s at every " Nothing
                   ,Tuple "candidate" (Just CandidateTerm)]
      highlightNgrams table input `shouldEqual` output
