module Gargantext.Components.Nodes.Corpus.Phylo
  ( phyloLayout
  ) where

import Gargantext.Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import DOM.Simple.Console (log2)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Gargantext.Components.PhyloExplorer.Types (PhyloDataset)
import Gargantext.Sessions (Session)
import Gargantext.Types (NodeID)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Simple.JSON as JSON
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Corpus.Phylo"

type Props =
  ( nodeId :: NodeID
  , session :: Session
  )

phyloLayout :: R2.Component Props
phyloLayout = R.createElement phyloLayoutCpt
phyloLayoutCpt :: R.Component Props
phyloLayoutCpt = here.component "phyloLayout" cpt where
  cpt _ _ = do

    fetchedDataBox <- T.useBox (Nothing :: Maybe PhyloDataset)
    fetchedData <- T.useLive T.unequal fetchedDataBox

    R.useEffectOnce' $ launchAff_ do
      result <- fetchPhyloJSON
      liftEffect $ case result of
        Left err -> log2 "error" err
        Right res -> T.write_ (Just res) fetchedDataBox

    pure $ case fetchedData of
      Nothing -> mempty
      Just fdata ->

        H.div
        { className:"phyloCorpus" }
        [ H.text $ show fdata ]

          -- ,
          --   infoCorpusR
          -- ,
          --   infoPhyloR
          -- ,
          --   timelineR
          -- ,
          --   isolineR
          -- ,
          --   wordcloudR
          -- ,
          --   phyloR


fetchPhyloJSON :: Aff (Either String PhyloDataset)
-- fetchPhyloJSON :: ReadForeign PhyloDataset => Aff Unit
fetchPhyloJSON =
  let
    request = AX.defaultRequest
      -- @WIP remove dumb data
      { url = "http://localhost:5000/js/knowledge-phylomemy.json"
      , method = Left GET
      , responseFormat = ResponseFormat.string
      }
  in do
    result <- request # AX.request
    liftEffect $ case result of
      Left err -> pure $ Left $ AX.printError err
      Right response -> case JSON.readJSON response.body of
        Left err -> pure $ Left $ show err
        Right (res :: PhyloDataset) -> pure $ Right res
