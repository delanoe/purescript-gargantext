module Gargantext.Pages.Home.States where

type State =
  { userName :: String
  , password :: String
  }

initialState :: State
initialState =
  { userName : ""
  , password : ""
  }
