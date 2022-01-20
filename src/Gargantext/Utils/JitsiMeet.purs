module Gargantext.Utils.JitsiMeet where

import Data.Function.Uncurried (Fn2, runFn2)
import DOM.Simple as DOM
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)

foreign import data JitsiMeet :: Type

type Jitsi =
  { parentNode :: DOM.Element
  , roomName :: String
  , width :: String
  , height :: String }

foreign import _api :: JitsiMeet
foreign import _jitsiMeetAPI :: EffectFn2 String Jitsi JitsiMeet

jitsiMeetAPI :: String -> Jitsi -> Effect JitsiMeet
jitsiMeetAPI = runEffectFn2 _jitsiMeetAPI

--jitsiMeetAPIFn :: String -> Jitsi -> JitsiMeet
--jitsiMeetAPIFn = runFn2 _jitsiMeetAPI
