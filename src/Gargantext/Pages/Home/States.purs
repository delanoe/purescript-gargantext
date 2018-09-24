module Gargantext.Pages.Home.States where

import Data.Newtype (class Newtype)

newtype State = State
  { userName :: String
  , password :: String
  }

derive instance newtypeState :: Newtype State _

initialState :: State
initialState = State
  { userName : ""
  , password : ""
  }
