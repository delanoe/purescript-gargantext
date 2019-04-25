module Gargantext.Components.Annotation.Utils where

import Gargantext.Types ( TermList(..) )

termClass :: TermList -> String
termClass GraphTerm = "graph-term"
termClass StopTerm = "stop-term"
termClass CandidateTerm = "candidate-term"
