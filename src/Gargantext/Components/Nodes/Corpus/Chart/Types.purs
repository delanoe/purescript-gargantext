module Gargantext.Components.Nodes.Corpus.Chart.Types where

import Data.Maybe (Maybe)
import Data.Tuple (Tuple)

import Gargantext.Sessions (Session)
import Gargantext.Types (TabType)
import Gargantext.Utils.Toestand as T2

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
    reload  :: T2.ReloadS
  | Props
)

type ReloadPath = Tuple T2.Reload (Record Path)
