module Gargantext.Components.Annotation.Types
  ( MenuType(..)
  , termClass
  )
  where

import Gargantext.Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Gargantext.Types (TermList(..))

data MenuType = NewNgram | SetTermListItem
derive instance Generic MenuType _
instance Eq MenuType where
  eq = genericEq

termClass :: TermList -> String
termClass CandidateTerm = "candidate-term"
termClass MapTerm       = "graph-term"
termClass StopTerm      = "stop-term"
