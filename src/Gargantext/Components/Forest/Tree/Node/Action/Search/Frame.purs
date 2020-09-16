module Gargantext.Components.Forest.Tree.Node.Action.Search.Frame where

import DOM.Simple as DOM
import DOM.Simple.Event (MessageEvent)
import DOM.Simple.EventListener (Callback, addEventListener, callback)
import DOM.Simple.Window (window)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.String (toLower)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Reactix as R
import Reactix.DOM.HTML as H
import URI.Extra.QueryPairs as NQP
import URI.Query as Query

import Gargantext.Components.Forest.Tree.Node.Action.Search.Types (DataField(..), Search, isIsTex_Advanced)
import Gargantext.Prelude (discard, identity, pure, unit, ($), (<>), (==), class Show, show)
import Gargantext.Utils.Reactix as R2

thisModule = "Gargantext.Components.Forest.Tree.Node.Action.Search.Frame"

--------------------

data FrameSource = Istex | Searx

derive instance genericFrameSource :: Generic FrameSource _

instance showFrameSource :: Show FrameSource where
  show = genericShow

--------------------
-- | Iframes

type SearchIFramesProps = (
    iframeRef :: R.Ref (Nullable DOM.Element)
  , search :: R.State Search
  )

searchIframes :: Record SearchIFramesProps -> R.Element
searchIframes props = R.createElement searchIframesCpt props []

searchIframesCpt :: R.Component SearchIFramesProps
searchIframesCpt = R2.hooksComponent thisModule "searchIframes" cpt
  where
    cpt { iframeRef, search: search@(search' /\ _) } _ = do
      pure $ if isIsTex_Advanced search'.datafield
         then divIframe { frameSource: Istex, iframeRef, search }
      else
        if Just Web == search'.datafield
           then divIframe { frameSource: Searx, iframeRef, search }
           else H.div {} []


type IFrameProps = (
    frameSource :: FrameSource
  , iframeRef :: R.Ref (Nullable DOM.Element)
  , search :: R.State Search
  )

divIframe :: Record IFrameProps -> R.Element
divIframe props = R.createElement divIframeCpt props []

divIframeCpt :: R.Component IFrameProps
divIframeCpt = R2.hooksComponent thisModule "divIframe" cpt
  where
    cpt { frameSource, iframeRef, search: search@(search' /\ _) } _ = do
      pure $ H.div { className: "frame-search panel panel-default" }
                   [ iframeWith { frameSource, iframeRef, search } ]

frameUrl :: FrameSource -> String
frameUrl Istex = "https://istex.frame.gargantext.org"
frameUrl Searx = "https://searx.frame.gargantext.org" -- 192.168.1.4:8080"


iframeWith :: Record IFrameProps -> R.Element
iframeWith props = R.createElement iframeWithCpt props []

iframeWithCpt :: R.Component IFrameProps
iframeWithCpt = R2.hooksComponent thisModule "iframeWith" cpt
  where
    cpt { frameSource, iframeRef, search: (search /\ setSearch) } _ =
      pure $ H.iframe { src: src frameSource search.term
                      , width: "100%"
                      , height: "100%"
                      , ref: iframeRef
                      , on: { load: \_ -> do
                                 addEventListener window "message" (changeSearchOnMessage url)
                                 R2.postMessage iframeRef search.term
                            }
                      } []
      where
        url :: String
        url =  frameUrl frameSource

        changeSearchOnMessage :: String -> Callback MessageEvent
        changeSearchOnMessage url' =
                callback $ \m -> if R2.getMessageOrigin m == url' then do
                                   let {url'', term} = R2.getMessageData m
                                   setSearch $ _ {url = url'', term = term}
                                 else
                                    pure unit

        isTexTermUrl :: String -> String
        isTexTermUrl term = url <> query
          where
            query = Query.print $ NQP.print identity identity qp

            qp = NQP.QueryPairs [ Tuple (NQP.keyFromString "query")
                                  (Just (NQP.valueFromString term))
                                ]

        searxTermUrl :: String -> String
        searxTermUrl term = url <> query
          where
            query = Query.print $ NQP.print identity identity qp
            qp = NQP.QueryPairs [ Tuple (NQP.keyFromString "q")
                                  (Just $ NQP.valueFromString term)
                                ]

        src :: FrameSource -> String -> String
        src Istex term = isTexTermUrl term
        src Searx term = searxTermUrl term
