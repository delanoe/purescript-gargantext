module Gargantext.Components.Nodes.Corpus.Chart.Types where

import Data.Maybe (Maybe)

import Gargantext.Sessions (Session)
import Gargantext.Types (TabType)

type Path = (
    corpusId :: Int
  , tabType  :: TabType
  )
type ListPath = (
    limit    :: Maybe Int
  , listId   :: Int
  | Path
)

type Props a = ( path :: Record a, session :: Session )
