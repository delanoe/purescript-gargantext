module Gargantext.Components.GraphExplorer.Frame.DocFocus
  ( docFocus
  ) where

import Gargantext.Prelude

import Data.Maybe (Maybe(..), isJust)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (Elevation(..))
import Gargantext.Components.Document.API (loadData)
import Gargantext.Components.Document.Layout (layout)
import Gargantext.Components.Document.Types (LoadedData, DocPath)
import Gargantext.Components.GraphExplorer.Types (GraphSideDoc(..))
import Gargantext.Config.REST (logRESTError)
import Gargantext.Hooks.Loader (useLoaderEffect)
import Gargantext.Hooks.Session (useSession)
import Gargantext.Sessions (Session)
import Gargantext.Types (CTabNgramType(..), TabSubType(..), TabType(..))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H


here :: R2.Here
here = R2.here "Gargantext.Components.GraphExplorer.Frame.DocFocus"

type Props =
  ( graphSideDoc  :: GraphSideDoc
  , closeCallback :: Unit -> Effect Unit
  )

docFocus :: R2.Leaf ( key :: String | Props )
docFocus = R2.leaf docFocusCpt
docFocusCpt :: R.Component ( key :: String | Props )
docFocusCpt = here.component "main" cpt where
  cpt { graphSideDoc: GraphSideDoc { docId, listId, corpusId }
      , closeCallback
      } _ = do
    session <- useSession
    -- | States
    -- |
    state' /\ state <- R2.useBox' (Nothing :: Maybe LoadedData)

    -- | Computed
    -- |
    let

      tabType :: TabType
      tabType = TabDocument (TabNgramType CTabTerms)

      path :: DocPath
      path =
        { listIds: [listId]
        , mCorpusId: Just corpusId
        , nodeId: docId
        , session
        , tabType
        }

    -- | Hooks
    -- |
    useLoaderEffect
      { errorHandler: logRESTError here "[docFocus]"
      , loader: loadData
      , path
      , state
      }

    -- | Render
    -- |
    pure $


      H.div
      { className: "graph-doc-focus" }
      [
        B.cloak
        { isDisplayed: isJust state'
        , idlingPhaseDuration: Just 150
        , cloakSlot:
            B.preloader
            {}

        , defaultSlot:
            R2.fromMaybe state' \loaded ->
              layout
              { loaded
              , path
              , sideControlsSlot: Just $
                  H.div
                  { className: "graph-doc-focus__header" }
                  [
                    B.iconButton
                    { name: "times"
                    , elevation: Level2
                    , callback: closeCallback
                    }
                  ]
              }
        }
      ]
