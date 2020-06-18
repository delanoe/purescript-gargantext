module Gargantext.Components.Forest.Tree.Node.Action.Download where

import Data.Maybe (Maybe(..))
import Gargantext.Components.Forest.Tree.Node.Tools (fragmentPT)
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
actionDownload NodeList id session = downloadButton href label info
    where
      href  = url session $ Routes.NodeAPI GT.NodeList (Just id) ""
      label = "Download List"
      info  = "Info about the List as JSON format"

actionDownload GT.Graph id session = downloadButton href label info
    where
      href  = url session $ Routes.NodeAPI GT.Graph (Just id) "gexf"
      label = "Download Graph"
      info  = "Info about the Graph as GEXF format"

actionDownload GT.Corpus id session = downloadButton href label info
    where
      href  = url session $ Routes.NodeAPI GT.Corpus (Just id) "export"
      label = "Download Corpus"
      info  = "Download as JSON"

actionDownload GT.Texts id session = downloadButton href label info
    where
      href  = url session $ Routes.NodeAPI GT.Texts (Just id) ""
      label = "Download texts"
      info  = "TODO: fix the backend route. What is the expected result ?"

actionDownload _ _ _ = pure $ fragmentPT $ "Soon, you will be able to dowload your file here "


type Href  = String
type Label = String
type Info  = String
downloadButton :: Href -> Label -> Info -> R.Hooks R.Element
downloadButton href label info = do
  pure $ R.fragment [ H.div { className: "row"}
                            [ H.div { className: "col-md-2"} []
                            , H.div { className: "col-md-7 flex-center"}
                                    [ H.p {} [H.text info] ]
                            ]
                    , H.span { className: "row" }
                             [ H.div { className: "panel-footer"}
                               [ H.div { className: "col-md-3"} []
                                       , H.div { className: "col-md-3 flex-center"}
                                               [ H.a { className: "btn btn-primary"
                                                     , style : { width: "50%" }
                                                     , href
                                                     , target: "_blank" }
                                                     [ H.text label ]
                                               ]
                                       ]
                               ]
                    ]

