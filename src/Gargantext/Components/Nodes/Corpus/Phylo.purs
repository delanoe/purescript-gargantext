module Gargantext.Components.Nodes.Corpus.Phylo
  ( phyloLayout
  ) where

import Gargantext.Prelude

import DOM.Simple (document, querySelector)
import Data.Maybe (Maybe(..))
import FFI.Simple ((..), (.=))
import Gargantext.Components.App.Data (Boxes)
import Gargantext.Components.PhyloExplorer.API (get)
import Gargantext.Components.PhyloExplorer.Layout (layout)
import Gargantext.Components.PhyloExplorer.Types (PhyloDataSet)
import Gargantext.Config.REST (logRESTError)
import Gargantext.Hooks.FirstEffect (useFirstEffect')
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Sessions (Session)
import Gargantext.Types (NodeID)
import Gargantext.Utils.Reactix as R2
import Reactix as R

type MainProps =
  ( nodeId      :: NodeID
  , session     :: Session
  , boxes       :: Boxes
  )

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Corpus.Phylo"

phyloLayout :: R2.Leaf MainProps
phyloLayout = R2.leaf phyloLayoutCpt

phyloLayoutCpt :: R.Component MainProps
phyloLayoutCpt = here.component "main" cpt where
  cpt { nodeId, session } _ = do

    let

      errorHandler = logRESTError here "[phylo]"

      handler (dataset :: PhyloDataSet) =
        content
        { nodeId
        , dataset
        }

    useLoader
      { errorHandler
      , loader: get session
      , path: nodeId
      , render: handler
      }


--------------------------------------------------------

type ContentProps =
  ( nodeId      :: NodeID
  , dataset     :: PhyloDataSet
  )

content :: R2.Leaf ContentProps
content = R2.leaf contentCpt

contentCpt :: R.Component ContentProps
contentCpt = here.component "content" cpt where
  cpt { nodeId, dataset } _ = do
  -- Hooks

    useFirstEffect' do
      -- @XXX: inopinent <div> (see Gargantext.Components.Router) (@TODO?)
      mEl <- querySelector document ".main-page__main-route .container"
      case mEl of
        Nothing -> pure unit
        Just el -> do
          style <- pure $ (el .. "style")
          pure $ (style .= "display") "none"
      -- @XXX: reset "main-page__main-route" wrapper margin
      --       see Gargantext.Components.Router) (@TODO?)
      mEl' <- querySelector document ".main-page__main-route"
      case mEl' of
        Nothing -> pure unit
        Just el -> do
          style <- pure $ (el .. "style")
          pure $ (style .= "padding") "initial"

  -- Render
    pure $

      layout
      { nodeId
      , phyloDataSet: dataset
      }
