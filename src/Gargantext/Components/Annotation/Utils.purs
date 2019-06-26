module Gargantext.Components.Annotation.Utils where

import Gargantext.Types ( TermList(..) )

termClass :: TermList -> String
termClass GraphTerm = "graph-term"
termClass StopTerm = "stop-term"
termClass CandidateTerm = "candidate-term"

termBootstrapClass :: TermList -> String
termBootstrapClass GraphTerm = "success"
termBootstrapClass StopTerm = "danger"
termBootstrapClass CandidateTerm = "info"
