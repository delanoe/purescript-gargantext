module Gargantext.Components.Annotation.Types
  ( MenuType(..)
  , termClass
  , ModeType(..)
  )
  where

import Gargantext.Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Gargantext.Types (TermList(..))

---------------------------------------------------------

data MenuType = NewNgram | SetTermListItem
derive instance Generic MenuType _
instance Eq MenuType where
  eq = genericEq

----------------------------------------------------------

termClass :: TermList -> String
termClass CandidateTerm = "candidate-term"
termClass MapTerm       = "graph-term"
termClass StopTerm      = "stop-term"

---------------------------------------------------------

data ModeType
  = EditionMode
  | AdditionMode

derive instance Generic ModeType _
instance Eq ModeType where eq = genericEq
instance Show ModeType where show = genericShow
instance Read ModeType where
  read :: String -> Maybe ModeType
  read = case _ of
    "EditionMode"  -> Just EditionMode
    "AdditionMode" -> Just AdditionMode
    _              -> Nothing
