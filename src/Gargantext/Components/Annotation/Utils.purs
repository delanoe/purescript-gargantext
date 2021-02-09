module Gargantext.Components.Annotation.Utils where

import Gargantext.Types ( TermList(..) )

termClass :: TermList -> String
termClass CandidateTerm = "candidate-term"
termClass MapTerm = "graph-term"
termClass StopTerm = "stop-term"

termBootstrapClass :: TermList -> String
-- termBootstrapClass CandidateTerm = "warning"
termBootstrapClass MapTerm = "success"
termBootstrapClass StopTerm = "danger"
termBootstrapClass CandidateTerm = "primary"
