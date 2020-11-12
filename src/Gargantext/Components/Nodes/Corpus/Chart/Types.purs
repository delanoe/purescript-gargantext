module Gargantext.Components.Nodes.Corpus.Chart.Types where

import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Reactix as R

import Gargantext.Sessions (Session)
import Gargantext.Types (ReloadS, TabType)

type Reload = Int

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
    reload  :: ReloadS
  | Props
)

type ReloadPath = Tuple Reload (Record Path)
