module Gargantext.Components.Lang where

import Gargantext.Prelude

import Data.Argonaut (class EncodeJson, encodeJson)
import Data.Array as A
import Data.Generic.Rep (class Generic)
import Data.Lens.Lens.Product (_1)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Foreign as F
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Show as GUS
import Gargantext.Utils.SimpleJSON as GJSON
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
           , DE
           , ES
           , IT
           , PL
           , CN
           , Universal
           , No_extraction
           ]

data Lang = FR | EN | DE | ES | IT | PL | CN | Universal | No_extraction
derive instance Generic Lang _
derive instance Ord Lang

instance Show Lang where
  show Universal     = "All"
  show No_extraction = "Nothing"
  show s             = genericShow s

langReader :: String -> Maybe Lang
langReader = GUS.reader allLangs

derive instance Eq Lang

-- instance EncodeJson Lang where
--   encodeJson a = encodeJson (show a)

instance JSON.ReadForeign Lang where
  readImpl f = do
    f' <- JSON.readImpl f
    case langReader f' of
      Nothing -> GJSON.throwJSONError $ F.ForeignError $ "Unknown language: " <> f'
      Just l  -> pure l
instance JSON.WriteForeign Lang where
  writeImpl l = JSON.writeImpl $ show l


data ServerType = CoreNLP | Spacy | JohnSnow
derive instance Generic ServerType _
instance Show ServerType where
  show = genericShow
instance JSON.ReadForeign ServerType where
  readImpl f = do
    f' <- JSON.readImpl f
    case f' of
      "CoreNLP"        -> pure CoreNLP
      "Spacy"          -> pure Spacy
      "JohnSnowServer" -> pure JohnSnow
      _                -> GJSON.throwJSONError $ F.ForeignError $ "Unknown server: " <> f'


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
