module Gargantext.Components.Forest.Tree.Node.Action.Download where

import Data.Maybe (Maybe(..))
import Gargantext.Components.Forest.Tree.Node.Action (Action(DownloadNode))
import Gargantext.Components.Forest.Tree.Node.Tools (fragmentPT, panel, submitButtonHref)
import Gargantext.Ends (url)
import Gargantext.Prelude (pure, ($))
import Gargantext.Routes as Routes
import Gargantext.Sessions (Session)
import Gargantext.Types (NodeType(..), ID)
import Gargantext.Types as GT
import Reactix as R
import Reactix.DOM.HTML as H

-- | Action : Download
actionDownload :: NodeType -> ID -> Session -> R.Hooks R.Element
actionDownload NodeList id session = pure $ panel [H.div {} [H.text info]]
                                           (submitButtonHref DownloadNode href)
    where
      href  = url session $ Routes.NodeAPI GT.NodeList (Just id) ""
      info  = "Info about the List as JSON format"

actionDownload GT.Graph id session = pure $ panel [H.div {} [H.text info]]
                                           (submitButtonHref DownloadNode href)
    where
      href  = url session $ Routes.NodeAPI GT.Graph (Just id) "gexf"
      info  = "Info about the Graph as GEXF format"

actionDownload GT.Corpus id session = pure $ panel [H.div {} [H.text info]]
                                           (submitButtonHref DownloadNode href)
    where
      href  = url session $ Routes.NodeAPI GT.Corpus (Just id) "export"
      info  = "Download as JSON"

{-
-- TODO fix the route
actionDownload GT.Texts id session = pure $ panel [H.div {} [H.text info]]
                                           (submitButtonHref DownloadNode href)
    where
      href  = url session $ Routes.NodeAPI GT.Texts (Just id) ""
      info  = "TODO: fix the backend route. What is the expected result ?"
      -}
actionDownload _ _ _ = pure $ fragmentPT $ "Soon, you will be able to download your file here "

