module Gargantext.Components.Annotation.Utils where

import Gargantext.Types ( TermList(..) )

termClass :: TermList -> String
termClass MapTerm = "graph-term"
termClass StopTerm = "stop-term"
termClass CandidateTerm = "candidate-term"

termBootstrapClass :: TermList -> String
termBootstrapClass MapTerm = "success"
termBootstrapClass StopTerm = "danger"
termBootstrapClass CandidateTerm = "warning"
