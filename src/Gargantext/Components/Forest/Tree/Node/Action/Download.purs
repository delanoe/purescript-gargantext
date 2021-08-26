module Gargantext.Components.Forest.Tree.Node.Action.Download where

import Data.Maybe (Maybe(..))
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Components.Forest.Tree.Node.Action (Action(DownloadNode))
import Gargantext.Components.Forest.Tree.Node.Tools (fragmentPT, panel, submitButtonHref)
import Gargantext.Ends (url)
import Gargantext.Prelude (pure, ($))
import Gargantext.Routes as Routes
import Gargantext.Sessions (Session)
import Gargantext.Types (ID)
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Action.Documentation"

-- | Action : Download
type ActionDownload =
  ( id       :: ID
  , nodeType :: GT.NodeType
  , session  :: Session )

actionDownload :: R2.Component ActionDownload
actionDownload = R.createElement actionDownloadCpt
actionDownloadCpt :: R.Component ActionDownload
actionDownloadCpt = here.component "actionDownload" cpt where
  cpt props@{ nodeType: GT.Corpus } _   = pure $ actionDownloadCorpus props []
  cpt props@{ nodeType: GT.Graph } _    = pure $ actionDownloadGraph props []
  cpt props@{ nodeType: GT.NodeList } _ = pure $ actionDownloadNodeList props []
  cpt props@{ nodeType: _ } _           = pure $ actionDownloadOther props []

actionDownloadCorpus :: R2.Component ActionDownload
actionDownloadCorpus = R.createElement actionDownloadCorpusCpt
actionDownloadCorpusCpt :: R.Component ActionDownload
actionDownloadCorpusCpt = here.component "actionDownloadCorpus" cpt where
  cpt { id, session } _ = do
    pure $ panel [H.div {} [H.text info]]
      (submitButtonHref DownloadNode href)
    where
      href  = url session $ Routes.NodeAPI GT.Corpus (Just id) "export"
      info  = "Download as JSON"

actionDownloadGraph :: R2.Component ActionDownload
actionDownloadGraph = R.createElement actionDownloadGraphCpt
actionDownloadGraphCpt :: R.Component ActionDownload
actionDownloadGraphCpt = here.component "actionDownloadGraph" cpt where
  cpt { id, session } _ = do
    pure $ panel [H.div {} [H.text info]]
      (submitButtonHref DownloadNode href)
    where
      href  = url session $ Routes.NodeAPI GT.Graph (Just id) "gexf"
      info  = "Info about the Graph as GEXF format"

actionDownloadNodeList :: R2.Component ActionDownload
actionDownloadNodeList = R.createElement actionDownloadNodeListCpt
actionDownloadNodeListCpt :: R.Component ActionDownload
actionDownloadNodeListCpt = here.component "actionDownloadNodeList" cpt where
  cpt { id, session } _ = do
    pure $ panel [ H.div {} [H.text info] ]
      (submitButtonHref DownloadNode href)
    where
      href  = url session $ Routes.NodeAPI GT.NodeList (Just id) ""
      info  = "Info about the List as JSON format"

{-
-- TODO fix the route
actionDownload GT.Texts id session = pure $ panel [H.div {} [H.text info]]
                                           (submitButtonHref DownloadNode href)
    where
      href  = url session $ Routes.NodeAPI GT.Texts (Just id) ""
      info  = "TODO: fix the backend route. What is the expected result ?"
      -}
actionDownloadOther :: R2.Component ActionDownload
actionDownloadOther = R.createElement actionDownloadOtherCpt
actionDownloadOtherCpt :: R.Component ActionDownload
actionDownloadOtherCpt = here.component "actionDownloadOther" cpt where
  cpt _ _ = do
    pure $ fragmentPT $ "Soon, you will be able to download your file here "
