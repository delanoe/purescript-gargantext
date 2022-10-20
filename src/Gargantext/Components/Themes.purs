module Gargantext.Components.Themes where

import Gargantext.Prelude

import Data.Array as A
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import FFI.Simple ((.=))
import Gargantext.Utils.Reactix (getElementById, (~~))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Themes"

stylesheetElId :: String
stylesheetElId = "bootstrap-css"

newtype Theme = Theme { location :: String
                      , name :: String }
derive instance Generic Theme _
instance Eq Theme where
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
darksterTheme = Theme { name: "darkster"
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

markThemeToDOMTree :: Theme -> Effect Unit
markThemeToDOMTree (Theme { name }) = do
  let setTheme el = pure $ (el ~~ "setAttribute") [ "data-theme", name ]
  getElementById "app"    >>= maybe R.nothing (setTheme)
  getElementById "portal" >>= maybe R.nothing (setTheme)


type ThemeSwitcherProps = (
    theme  :: T.Box Theme
  , themes :: Array Theme
  )

themeSwitcher :: R2.Component ThemeSwitcherProps
themeSwitcher = R.createElement themeSwitcherCpt

themeSwitcherCpt :: R.Component ThemeSwitcherProps
themeSwitcherCpt = here.component "themeSwitcher" cpt
  where
    cpt { theme, themes } _ = do
      currentTheme <- T.useLive T.unequal theme

      let option (Theme { name }) = H.option { value: name } [ H.text name ]
      let options = map option themes

      R.useEffectOnce' $ markThemeToDOMTree currentTheme

      pure $ R2.select { className: "form-control"
                       , defaultValue: themeName currentTheme
                       , on: { change: onChange theme } } options
      where
        onChange box e = do
          let value = R.unsafeEventValue e
          let mTheme = A.head $ A.filter (\(Theme { name }) -> value == name) themes

          case mTheme of
            Nothing -> pure unit
            Just t  -> do
              switchTheme t
              markThemeToDOMTree t
              T.write_ t box
