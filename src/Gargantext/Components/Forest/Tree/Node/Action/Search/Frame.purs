module Gargantext.Components.Forest.Tree.Node.Action.Search.Frame where

import DOM.Simple as DOM
import DOM.Simple.Event (MessageEvent)
import DOM.Simple.EventListener (Callback, addEventListener, callback)
import DOM.Simple.Window (window)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T
import URI.Extra.QueryPairs as NQP
import URI.Query as Query

import Gargantext.Prelude

import Gargantext.Components.Forest.Tree.Node.Action.Search.Types
  ( DataField(..), Search, isIsTex_Advanced )
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Tree.Node.Action.Search.Frame"

--------------------

data FrameSource = Istex | Searx

derive instance genericFrameSource :: Generic FrameSource _

instance showFrameSource :: Show FrameSource where
  show = genericShow

--------------------
-- | Iframes

type SearchIFramesProps = (
    iframeRef :: R.Ref (Nullable DOM.Element)
  , search    :: T.Box Search
  )

searchIframes :: R2.Component SearchIFramesProps
searchIframes = R.createElement searchIframesCpt

searchIframesCpt :: R.Component SearchIFramesProps
searchIframesCpt = here.component "searchIframes" cpt
  where
    cpt { iframeRef, search } _ = do
      search' <- T.useLive T.unequal search

      pure $ if isIsTex_Advanced search'.datafield
         then divIframe { frameSource: Istex, iframeRef, search } []
      else
        if Just Web == search'.datafield
           then divIframe { frameSource: Searx, iframeRef, search } []
           else H.div {} []


type IFrameProps = (
    frameSource :: FrameSource
  , iframeRef   :: R.Ref (Nullable DOM.Element)
  , search      :: T.Box Search
  )

divIframe :: R2.Component IFrameProps
divIframe = R.createElement divIframeCpt

divIframeCpt :: R.Component IFrameProps
divIframeCpt = here.component "divIframe" cpt
  where
    cpt props _ = do
      pure $ H.div { className: "frame-search card" }
                   [ iframeWith props [] ]

frameUrl :: FrameSource -> String
frameUrl Istex = "https://istex.frame.gargantext.org"
frameUrl Searx = "https://searx.frame.gargantext.org" -- 192.168.1.4:8080"


iframeWith :: R2.Component IFrameProps
iframeWith = R.createElement iframeWithCpt

iframeWithCpt :: R.Component IFrameProps
iframeWithCpt = here.component "iframeWith" cpt
  where
    cpt { frameSource, iframeRef, search } _ = do
      search' <- T.useLive T.unequal search

      pure $ H.iframe { src: src frameSource search'.term
                      , width: "100%"
                      , height: "100%"
                      , ref: iframeRef
                      , on: { load: \_ -> do
                                 addEventListener window "message" (changeSearchOnMessage url)
                                 R2.postMessage iframeRef search'.term
                            }
                      } []
      where
        url :: String
        url =  frameUrl frameSource

        changeSearchOnMessage :: String -> Callback MessageEvent
        changeSearchOnMessage url' =
                callback $ \m -> if R2.getMessageOrigin m == url' then do
                                   let {url'', term} = R2.getMessageData m
                                   T.modify_ (_ {url = url'', term = term}) search
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
