module Gargantext.Components.Frame.Layout
  ( layout
  ) where

import Gargantext.Prelude

import DOM.Simple (document, querySelector)
import DOM.Simple as DOM
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, null, toMaybe)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (Variant(..))
import Gargantext.Components.FolderView as FV
import Gargantext.Components.Frame.Types (Base, Hyperdata(..), FrameId)
import Gargantext.Components.Node (NodePoly(..))
import Gargantext.Types (NodeType(..))
import Gargantext.Utils.JitsiMeet as JM
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Reactix.DOM.HTML as H
import Web.URL as WURL

here :: R2.Here
here = R2.here "Gargantext.Components.Frame.Layout"

type Props =
  ( frame    :: NodePoly Hyperdata
  , reload   :: T2.ReloadS
  , nodeId   :: Int
  , nodeType :: NodeType
  )

layout :: R2.Leaf Props
layout = R2.leaf layoutCpt

layoutCpt :: R.Component Props
layoutCpt = here.component "main" cpt where
  cpt { frame: NodePoly { hyperdata: h@(Hyperdata { base, frame_id }) }
      , nodeId
      , nodeType
      , reload
      } _ = case nodeType of

    -- Visio Node
    NodeFrameVisio ->

      case WURL.fromAbsolute base of

        Nothing  ->

          pure $

            B.caveat
            { variant: Warning }
            [
              H.text $ "Wrong base url: " <> base
            ]

        Just url ->

          -- pure $ nodeFrameVisio' { frame_id, reload, url }
          pure $

            H.div
            {}
            [
              B.h1_ "Visio Room"
            ,
              H.a
              { className : "fa fa-video-camera fa-5x"
              , href : hframeUrl nodeType base frame_id
              , target: "_blank"
              }
              []
            ,
              B.p_ "Click on the Camera logo to access to your room"
            ,
              B.p_ "This a unique room dedicated to your team"
            ,
              B.p_ "Works with Chromium/Chrome only for now."
            ]

    -- Other Frame Nodes
    _ -> do

      -- @XXX: Runtime odd behavior
      --       cannot use the `useEffect` + its cleanup function within the
      --       same `Effect`, otherwise the below cleanup example will be
      --       execute at mount

      -- @XXX: inopinent <div> (see Gargantext.Components.Router) (@TODO?)
      R.useEffectOnce' do
        mEl <- querySelector document ".main-page__main-route .container"

        case mEl of
          Nothing -> R.nothing
          Just el -> R2.addClass el [ "d-none" ]

      R.useEffectOnce do
        pure do
          mEl <- querySelector document ".main-page__main-route .container"

          case mEl of
            Nothing -> R.nothing
            Just el -> R2.removeClass el [ "d-none" ]

      -- @XXX: reset "main-page__main-route" wrapper margin
      --       see Gargantext.Components.Router) (@TODO?)
      R.useEffectOnce' do
        mEl <- querySelector document ".main-page__main-route"

        case mEl of
          Nothing -> R.nothing
          Just el -> R2.addClass el [ "p-0" ]

      R.useEffectOnce do
        pure do
          mEl <- querySelector document ".main-page__main-route"

          case mEl of
            Nothing -> R.nothing
            Just el -> R2.removeClass el [ "p-0" ]

      pure $

        H.div
        { className: "frame-layout"
        , rows: "100%,*"
        }
        [
          -- H.script { src: "https://visio.gargantext.org/external_api.js"} [],
          H.iframe
          { src: hframeUrl nodeType base frame_id
          , width: "100%"
          , height: "100%"
          }
          []
        ]

--------------------------------------------------------------

type NodeFrameVisioProps =
  ( frame_id  :: String
  , reload    :: T2.ReloadS
  , url       :: WURL.URL
  )

nodeFrameVisio :: R2.Leaf NodeFrameVisioProps
nodeFrameVisio = R2.leaf nodeFrameVisioCpt

nodeFrameVisioCpt :: R.Component NodeFrameVisioProps
nodeFrameVisioCpt = here.component "nodeFrameVisio" cpt where
  cpt { frame_id
      , url
      } _  = do
    ref <- R.useRef (null :: Nullable DOM.Element)

    R.useEffect' $ do
      here.log2 "[nodeFrameVisio] ref" $ R.readRef ref
      here.log2 "[nodeFrameVisio] JM.api" JM._api
      case toMaybe (R.readRef ref) of
        Nothing -> pure unit
        Just r  -> do
          api <- JM.jitsiMeetAPI (WURL.host url) { parentNode: r
                                                  , roomName: frame_id
                                                  , width: "100%"
                                                  , height: "100%" }
          here.log2 "[nodeFrameVisio] api" api

    pure $ H.div { ref, className: "jitsi-iframe" } [ ]

--------------------------------------------------------------

hframeUrl :: NodeType -> Base -> FrameId -> String
hframeUrl NodeFrameNotebook _ frame_id = frame_id  -- Temp fix : frame_id is currently the whole url created
hframeUrl NodeFrameCalc  base frame_id = base <> "/" <> frame_id
hframeUrl NodeFrameVisio base frame_id = base <> "/" <> frame_id
hframeUrl _ base frame_id = base <> "/" <> frame_id <> "?view" -- "?both"
