module Gargantext.Components.Forest.Tree.Node.Action.Search.Frame where

import DOM.Simple as DOM
import DOM.Simple.Event (MessageEvent)
import DOM.Simple.EventListener (Callback, addEventListener, callback)
import DOM.Simple.Window (window)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Gargantext.Components.Forest.Tree.Node.Action.Search.Types (DataField(..), Search, isIsTex_Advanced)
import Gargantext.Prelude (discard, identity, pure, unit, ($), (<>), (==))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import URI.Extra.QueryPairs as NQP
import URI.Query as Query

--------------------
-- | Iframes
searchIframes :: R.State Search
              -> R.Ref (Nullable DOM.Element)
              -> R.Element
searchIframes search@(search' /\ _) iframeRef =
  if isIsTex_Advanced search'.datafield then
    H.div { className: "istex-search panel panel-default" }
          [ iframeWith "https://istex.gargantext.org" search iframeRef ]
  else
    if Just Web == search'.datafield then
      H.div { className: "istex-search panel panel-default" }
            [ iframeWith "https://searx.gargantext.org" search iframeRef ]
    else
      H.div {} []

iframeWith :: String
           -> R.State Search
           -> R.Ref (Nullable DOM.Element)
           -> R.Element
iframeWith url (search /\ setSearch) iframeRef =
  H.iframe { src: isTexTermUrl search.term
           , width: "100%"
           , height: "100%"
           , ref: iframeRef
           , on: { load: \_ -> do
                   addEventListener window "message" (changeSearchOnMessage url)
                   R2.postMessage iframeRef search.term
                 }
          } []
  where
    changeSearchOnMessage :: String -> Callback MessageEvent
    changeSearchOnMessage url' =
      callback $ \m -> if R2.getMessageOrigin m == url'
                         then do
                           let {url'', term} = R2.getMessageData m
                           setSearch $ _ {url = url'', term = term}
                         else
                           pure unit
    isTexTermUrl term = url <> query
      where
        query = Query.print $ NQP.print identity identity qp

        qp = NQP.QueryPairs [ Tuple (NQP.keyFromString "query")
                                    (Just (NQP.valueFromString term))
                            ]

