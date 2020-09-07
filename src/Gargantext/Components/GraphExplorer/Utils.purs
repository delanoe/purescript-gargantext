module Gargantext.Components.GraphExplorer.Utils
where

import Gargantext.Prelude

import Gargantext.Components.GraphExplorer.Types as GET
import Gargantext.Hooks.Sigmax.Types as ST


stEdgeToGET :: Record ST.Edge -> GET.Edge
stEdgeToGET { _original } = _original

stNodeToGET :: Record ST.Node -> GET.Node
stNodeToGET { id, label, x, y, _original: GET.Node { attributes, size, type_ } } = GET.Node {
    attributes
  , id_: id
  , label
  , size
  , type_
  , x
  , y
  }
