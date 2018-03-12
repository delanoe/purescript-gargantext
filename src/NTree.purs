module NTree where

import Prelude

import Data.Tuple (Tuple(..))
import React (ReactElement)
import React.DOM (a, i, li, text, ul)
import React.DOM.Props (className, href)

data NTree a = NLeaf a | NNode String (Array (NTree a))

type FTree = NTree (Tuple String String)


toHtml :: FTree -> ReactElement
toHtml (NLeaf (Tuple name link)) =
  li []
  [ a [ href link]
    [ i [className "fas fa-folder"] []
    , text name
    ]
  ]
toHtml (NNode name ary) =
  ul []
  [ li [] $
    [ text name
    ] <>
    map toHtml ary
  ]
