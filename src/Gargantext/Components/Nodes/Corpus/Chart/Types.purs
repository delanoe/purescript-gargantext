module Gargantext.Components.Nodes.Corpus.Chart.Types where

import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Effect (Effect)
import Gargantext.Components.App.Data (Boxes)
import Gargantext.Components.Charts.Options.Type (EChartsInstance, MouseEvent)
import Gargantext.Prelude (Unit)
import Gargantext.Sessions (Session)
import Gargantext.Types (FrontendError, TabType)
import Gargantext.Utils.Toestand as T2
import Toestand as T

type Path = (
    corpusId :: Int
  , limit    :: Maybe Int
  , listId   :: Int
  , tabType  :: TabType
  )

type Props = (
    boxes   :: Boxes
  , path    :: Record Path
  , session :: Session
  , onClick :: Maybe (MouseEvent -> Effect Unit)
  , onInit  :: Maybe (EChartsInstance -> Effect Unit)
  )

type MetricsProps = (
    reload  :: T2.ReloadS
  | Props
)

type ReloadPath = Tuple T2.Reload (Record Path)
