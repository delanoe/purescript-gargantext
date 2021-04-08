module Gargantext.Components.Themes where

import Data.Array as A
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import FFI.Simple ((.=))
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.Prelude
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Themes"

stylesheetElId :: String
stylesheetElId = "bootstrap-css"

newtype Theme = Theme { location :: String
                      , name :: String }
derive instance genericTheme :: Generic Theme _
instance genericEq :: Eq Theme where
  eq = genericEq

themeName :: Theme -> String
themeName (Theme { name }) = name

defaultTheme :: Theme
defaultTheme = Theme { name: "default"
                     , location: "styles/bootstrap-default.css" }

greysonTheme :: Theme
greysonTheme = Theme { name: "greyson"
                     , location: "styles/bootstrap-greyson.css" }

monotonyTheme :: Theme
monotonyTheme = Theme { name: "monotony"
                      , location: "styles/bootstrap-monotony.css" }

herbieTheme :: Theme
herbieTheme = Theme { name: "herbie"
                      , location: "styles/bootstrap-herbie.css" }

darksterTheme :: Theme
darksterTheme = Theme { name: "darkster (bêta)"
                      , location: "styles/bootstrap-darkster.css" }

allThemes :: Array Theme
allThemes = [ defaultTheme, greysonTheme, monotonyTheme, herbieTheme, darksterTheme]

switchTheme :: Theme -> Effect Unit
switchTheme (Theme { location }) = do
  mEl <- R2.getElementById stylesheetElId
  case mEl of
    Nothing -> pure unit
    Just el -> do
      _ <- pure $ (el .= "href") location
      pure unit

type ThemeSwitcherProps = (
    theme  :: Theme
  , themes :: Array Theme
  )

themeSwitcher :: R2.Component ThemeSwitcherProps
themeSwitcher = R.createElement themeSwitcherCpt

themeSwitcherCpt :: R.Component ThemeSwitcherProps
themeSwitcherCpt = here.component "themeSwitcher" cpt
  where
    cpt { theme, themes } _ = do
      currentTheme <- T.useBox theme
      currentTheme' <- T.useLive T.unequal currentTheme

      let option (Theme { name }) = H.option { value: name } [ H.text name ]
      let options = map option themes

      pure $ R2.select { className: "form-control"
                       , defaultValue: themeName currentTheme'
                       , on: { change: onChange currentTheme } } options
      where
        onChange currentTheme e = do
          let value = R.unsafeEventValue e
          let mTheme = A.head $ A.filter (\(Theme { name }) -> value == name) themes

          case mTheme of
            Nothing -> pure unit
            Just t  -> do
              switchTheme t
              T.write_ t currentTheme