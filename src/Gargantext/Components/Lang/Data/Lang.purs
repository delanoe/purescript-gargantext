module Gargantext.Components.Data.Lang where

import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Maybe (Maybe(..))

import Gargantext.Prelude (class Eq, class Show, show)

-- Language used for search
allLangs :: Array Lang
allLangs = [ EN
           , FR
           , Universal
           , No_extraction
           ]

data Lang = FR | EN | Universal | No_extraction

instance showLang :: Show Lang where
  show FR = "FR"
  show EN = "EN"
  show Universal = "All"
  show No_extraction = "Nothing"

derive instance eqLang :: Eq Lang

readLang :: String -> Maybe Lang
readLang "FR"  = Just FR
readLang "EN"  = Just EN
readLang "All" = Just Universal
readLang "Nothing" = Just No_extraction
readLang _           = Nothing

instance encodeJsonLang :: EncodeJson Lang where
  encodeJson a = encodeJson (show a)


-- Language used for the landing page
data LandingLang = LL_EN | LL_FR
