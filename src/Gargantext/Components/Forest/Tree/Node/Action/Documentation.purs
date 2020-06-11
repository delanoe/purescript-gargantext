module Gargantext.Components.Forest.Tree.Node.Action.Documentation where

import Gargantext.Prelude (map, pure, show, ($), (<>))
import Gargantext.Types (NodeType)
import Gargantext.Types as GT
import Reactix as R
import Reactix.DOM.HTML as H

-- | Action: Show Documentation
actionDoc :: NodeType -> R.Hooks R.Element
actionDoc nodeType =
  pure $ R.fragment [ H.div { style: {margin: "10px"} }
                            $ [ infoTitle nodeType ]
                            <> (map (\info -> H.p {} [H.text info]) $ docOf nodeType)
                    ]
  where
    infoTitle :: NodeType -> R.Element
    infoTitle nt = H.div { style: {margin: "10px"}}
                         [ H.h3 {} [H.text "Documentation about " ]
                         , H.h3 {className: GT.fldr nt true} [ H.text $ show nt ]
                         ]

-- | TODO add documentation of all NodeType
docOf :: NodeType -> Array String
docOf GT.NodeUser = [ "This account is personal"
                    , "See the instances terms of uses."
                    ]
docOf GT.FolderPrivate = ["This folder and its children are private only."]
docOf GT.FolderPublic  = ["Soon, you will be able to build public folders to share your work with the world!"]
docOf GT.FolderShared  = ["Soon, you will be able to build teams folders to share your work"]
docOf nodeType         = ["More information on " <> show nodeType]



