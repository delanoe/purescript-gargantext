module Gargantext.Components.Corpus.EditionBlock
  ( editionBlock
  ) where

import Gargantext.Prelude

import Data.Maybe (Maybe(..), isJust)
import Data.Tuple.Nested ((/\))
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Corpus.CodeSection (loadCorpus')
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Components.Nodes.Corpus.Types (CorpusInfo(..), Hyperdata(..), getCorpusInfo)
import Gargantext.Components.Table (tableHeaderEditionBlock)
import Gargantext.Config.REST (logRESTError)
import Gargantext.Hooks.Loader (useLoaderEffect)
import Gargantext.Hooks.Session (useSession)
import Gargantext.Types (ID)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

type Props =
  ( nodeId :: ID
  )

here :: R2.Here
here = R2.here "Gargantext.Components.Corpus.EditionBlock"

editionBlock :: R2.Leaf Props
editionBlock = R2.leaf editionBlockCpt
editionBlockCpt :: R.Component Props
editionBlockCpt = here.component "main" cpt where
  cpt { nodeId
      } _ = do
    -- | States
    -- |
    session <- useSession

    state' /\ state <- R2.useBox' Nothing

    -- | Computed
    -- |
    let
      errorHandler = logRESTError here "[corpusLayout]"

    -- | Hooks
    -- |
    useLoaderEffect
      { errorHandler
      , loader: loadCorpus'
      , path: { nodeId, session }
      , state
      }

    -- | Render
    -- |
    pure $

      B.cloak
      { isDisplayed: isJust state'
      , cloakSlot:

          -- (?) quick and dirty placeholder for the <tableHeaderEditionBlock>
          H.div
          { style:
            { width: "100%"
            , height: "136px"
            , position: "relative"
            }
          }
          [
            B.preloader
            {}
          ]

      , defaultSlot:
          R2.fromMaybe state' \hyperdata ->

            editionBlock_
            { nodeId
            , hyperdata
            }
      }

----------------------------------------------------------------


type Props_ =
  ( nodeId      :: ID
  , hyperdata   :: NodePoly Hyperdata
  )

editionBlock_ :: R2.Leaf Props_
editionBlock_ = R2.leaf editionBlockCpt_
editionBlockCpt_ :: R.Component Props_
editionBlockCpt_ = here.component "main_" cpt where
  cpt { nodeId
      , hyperdata
      } _ = do
    -- | Computed
    -- |
    let
      NodePoly { name, date, hyperdata } = hyperdata

      Hyperdata h = hyperdata
      corpusInfo = getCorpusInfo h.fields

      CorpusInfo { title, desc, query, authors } = corpusInfo

    -- -- | States
    -- -- |
    corpusInfoS <- T.useBox corpusInfo

    session <- useSession

    -- -- | Render
    -- -- |
    pure $

      tableHeaderEditionBlock
      { hyperdata
      , nodeId
      , session
      , corpusInfoS
      , defaultData:
        { name
        , date
        , query
        , desc
        , title
        , authors
        }
      }
