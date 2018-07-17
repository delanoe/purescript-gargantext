module Gargantext.Pages.Home.States where

import Prelude hiding (div)

import Control.Monad.Cont.Trans (lift)
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Gargantext.Components.Lang.Landing.EnUS as En
import Gargantext.Components.Lang.Landing.FrFR as Fr
import Gargantext.Components.Data.Landing (BlockText(..), BlockTexts(..), Button(..), LandingData(..))
import Gargantext.Components.Data.Lang (Lang(..))
import Network.HTTP.Affjax (AJAX)
import React (ReactElement)
import React.DOM (a, div, h3, i, img, p, span, text)
import React.DOM.Props (Props, _id, aria, className, href, src, target, title)
import Routing.Hash.Aff (setHash)
import Thermite (PerformAction, Render, Spec, modifyState, simpleSpec)



newtype State = State
  { userName :: String
  , password :: String
  }


initialState :: State
initialState = State
  {userName : ""
 , password : ""
  }


