module Gargantext.Components.Lang where

import Gargantext.Prelude

import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Array as A
import Data.Generic.Rep (class Generic)
import Data.Lens.Lens.Product (_1)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Simple.JSON as JSON
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Lang"

-- Language used for search
allLangs :: Array Lang
allLangs = [ EN
           , FR
           , Universal
           , No_extraction
           ]

data Lang = FR | EN | Universal | No_extraction

instance Show Lang where
  show FR            = "FR"
  show EN            = "EN"
  show Universal     = "All"
  show No_extraction = "Nothing"

derive instance Eq Lang

instance Read Lang where
  read "FR"      = Just FR
  read "EN"      = Just EN
  read "All"     = Just Universal
  read "Nothing" = Just No_extraction
  read _         = Nothing


instance EncodeJson Lang where
  encodeJson a = encodeJson (show a)

instance JSON.WriteForeign Lang where
  writeImpl l = JSON.writeImpl $ show l

-- Language used for the landing page
data LandingLang = LL_EN | LL_FR

derive instance Eq LandingLang

instance Show LandingLang where
  show LL_EN = "EN"
  show LL_FR = "FR"

-- @TODO a possible method/class that a real i18n logic could later replace
class Show t <= Translate t where
  translate :: Lang -> t -> String

allFeLangs :: Array LandingLang
allFeLangs = [ LL_EN, LL_FR ]

type LangSwitcherProps = (
    lang  :: T.Box LandingLang
  , langs :: Array LandingLang
)

langSwitcher :: R2.Component LangSwitcherProps
langSwitcher = R.createElement langSwitcherCpt

langSwitcherCpt :: R.Component LangSwitcherProps
langSwitcherCpt = here.component "langSwitcher" cpt
  where
    cpt { lang, langs} _ = do
      currentLang <- T.useLive T.unequal lang
      let option l = H.option { value: show l} [ H.text $ show l]
      let options = map option langs

      pure $ R2.select { className: "form-control"
                       , defaultValue: show currentLang
                       , on: {change: onChange lang } } options
      where
        onChange box e = do
          let value = R.unsafeEventValue e
          let mLang = A.head $ A.filter (\l -> value == show l) langs

          case mLang of
            Nothing -> pure unit
            Just l  -> do
              T.write_ l box
