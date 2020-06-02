module Gargantext.Components.Nodes.Corpus.Chart.Types where

import Data.Maybe (Maybe)
import Reactix as R

import Gargantext.Sessions (Session)
import Gargantext.Types (TabType)

type Path = (
    corpusId :: Int
  , limit    :: Maybe Int
  , listId   :: Int
  , tabType  :: TabType
  )

type Props = (
    path :: Record Path
  , session :: Session
  )

type MetricsProps = (
    reload  :: R.State Int
  | Props
)
