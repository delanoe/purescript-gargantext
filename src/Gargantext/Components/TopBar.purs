module Gargantext.Components.TopBar (component) where

import Gargantext.Prelude

import Data.Foldable (intercalate)
import Effect (Effect)
import Gargantext.Components.App.Store as AppStore
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ButtonVariant(..), Variant(..))
import Gargantext.Components.Lang (langSwitcher, allFeLangs)
import Gargantext.Components.Themes (themeSwitcher, allThemes)
import Gargantext.Types (Handed(..), defaultCacheParams)
import Gargantext.Utils (setter, (?))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.TopBar"

component :: R2.Leaf ()
component = R2.leaf componentCpt
componentCpt :: R.Component ()
componentCpt = here.component "main" cpt where
  cpt _ _ = do
    -- | States
    -- |
    { lang
    , showTree
    , theme
    } <- AppStore.use

    showTree' <- R2.useLive' showTree

    -- | Effects
    -- |

    -- | Behaviors
    -- |
    let
      onTreeToggleClick _ = do
        let new = not showTree'
        T.write_ new showTree
        -- transfer local Component change to Local Storage
        onTreeToggleChange new

    -- | Render
    -- |
    pure $

      H.div
      { className: "main-topbar navbar navbar-expand-lg navbar-dark bg-dark"
      , id: "dafixedtop"
      , role: "navigation"
      }
      [
        logo
      ,
        divDropdownLeft {} []
      ,
        H.li
        { title: "If you are Left Handed you can change\n"
              <> "the interface by clicking on me. Click\n"
              <> "again to come back to previous state."
        , className: "nav-item main-topbar__hand-button"
        }
        [
          handedChooser
          {}
        ]
      ,
        H.li
        { title: "Hello! Looking for the tree ?\n"
              <> "Just watch on the other side!\n"
              <> "Click on the hand again to see it back."
        , className : "nav-item main-topbar__help-button"
        }
        [
          H.a
          { className: "nav-link navbar-text" }
          [
            H.span
            { className: "fa fa-question-circle-o" } []
          ]
        ]
      ,
        H.li
        { className: "nav-item main-topbar__theme-switcher" }
        [
          themeSwitcher
          { theme
          , themes: allThemes
          } []
        ]

{-      ,
        H.li { className: "nav-item main-topbar__lang-switcher" }
          [ langSwitcher
            { lang
            , langs: allFeLangs
            } []
          ]-}
        , B.button
          { variant: showTree' ?
              ButtonVariant Light $
              OutlinedButtonVariant Light
          , callback: onTreeToggleClick
          , className: "main-topbar__tree-switcher"
          }
          [
            if showTree'
            then H.text "Hide Tree"
            else H.text "Show Tree"
          ]
        , H.div { id: "portal-topbar" } []
      ]

      -- SB.searchBar {session, databases: allDatabases}

onTreeToggleChange :: Boolean -> Effect Unit
onTreeToggleChange new = do
  cache <- R2.loadLocalStorageState' R2.appParamsKey defaultCacheParams
  let update = setter (_ { showTree = new }) cache
  R2.setLocalStorageState R2.appParamsKey update

----------------------------------------------------------

logo :: R.Element
logo =
  H.a { className, href: "#/" } [
    H.img { src, title, width: "30", height: "28" }
  ]
  where
    className = "main-topbar__logo navbar-brand logoSmall"
    src       = "images/logoSmall.png"
    title     = "Back home."


divDropdownLeft :: R2.Component ()
divDropdownLeft = R.createElement divDropdownLeftCpt
divDropdownLeftCpt :: R.Component ()
divDropdownLeftCpt = here.component "divDropdownLeft" cpt
  where
    cpt {} _ = do
      show <- T.useBox false

      pure $ H.li { className: "nav-item dropdown" } [
          menuButton { element: menuElement, show } []
        , menuElements { elements, show } []
        ]

    menuElement = LiNav { title : "About Gargantext"
                        , href  : "#"
                        , icon  : "fa fa-info-circle"
                        , text  : "Info" }

    elements = [
      [
        LiNav { title : "Quick start, tutorials and methodology"
              , href  : "https://iscpif.fr/gargantext/your-first-map/"
              , icon  : "fa fa-lightbulb-o"
              , text  : "Tutorials"
              }

      , LiNav { title : "Learn with us!"
              , href  : "https://iscpif.fr/projects/gargantext/"
              , icon  : "fa fa-life-ring"
              , text  : "Subscribe to Garg Lessons"
              }

      , LiNav { title : "Report bug here"
              , href  : "https://www.iscpif.fr/gargantext/feedback-and-bug-reports/"
              , icon  : "fa fa-bullhorn"
              , text  : "Feedback"
              }
      ]
      , -----------------------------------------------------------
      [ LiNav { title : "Chat"
              , href  : "https://chat.iscpif.fr/channel/gargantext"
              , icon  : "fa fa-rocket"
              , text  : "Chat"
              }
      , LiNav { title : "Forums"
              , href  : "https://discourse.iscpif.fr/c/gargantext"
              , icon  : "fa fa-weixin"
              , text  : "Forum"
              }
      ]
      ,------------------------------------------------------------

      [ LiNav { title : "Changelog"
              , href  : "https://gitlab.iscpif.fr/gargantext/main/blob/master/CHANGELOG.md"
              , icon  : "fa fa-clock-o"
              , text  : "Versions change"
              }
      , LiNav { title : "Code documentation"
              , href  : "https://doc.gargantext.org"
              , icon  : "fa fa-book"
              , text  : "Source Code Documentation"
              }
      , LiNav { title : "API documentation"
              , href  : "https://cnrs.gargantext.org/swagger-ui"
              , icon  : "fa fa-code-fork"
              , text  : "API documentation"
              }
      , LiNav { title : "Source code"
              , href  : "https://gitlab.iscpif.fr/gargantext/haskell-gargantext"
              , icon  : "fa fa-code"
              , text  : "Source Code"
              }
      ]

      ,------------------------------------------------------------
      [ LiNav { title : "Learn more about us"
              , href  : "https://iscpif.fr/projects/gargantext/"
              , icon  : "fa fa-question"
              , text  : "About"
              }
      ]
    ] -- ===========================================================

type MenuButtonProps = (
    element :: LiNav
  , show    :: T.Box Boolean
  )

menuButton :: R2.Component MenuButtonProps
menuButton = R.createElement menuButtonCpt
menuButtonCpt :: R.Component MenuButtonProps
menuButtonCpt = here.component "menuButton" cpt
  where
    cpt { element: LiNav { icon, text, title }, show } _ = do
      pure $ H.a { className: "dropdown-toggle navbar-text"
                -- , data: {toggle: "dropdown"}
                , title
                , on: { click: \_ -> T.modify_ not show }
                , role: "button" } [
          H.span { aria: {hidden : true}, className: icon } []
        , H.text (" " <> text)
        ]

-- | Menu in the sidebar, syntactic sugar
type MenuElementsProps = (
    elements :: Array (Array LiNav)
  , show     :: T.Box Boolean
  )

menuElements :: R2.Component MenuElementsProps
menuElements = R.createElement menuElementsCpt
menuElementsCpt :: R.Component MenuElementsProps
menuElementsCpt = here.component "menuElements" cpt
  where
    cpt { elements, show } _ = do
      show' <- T.useLive T.unequal show

      pure $ if show' then
               H.ul { className: "dropdown-menu"
                    , on: { click: \_ -> T.write_ false show }
                    , style: { display: "block" } } $ intercalate divider $ map (map liNav) elements
             else
               H.div {} []
    divider :: Array R.Element
    divider = [H.li {className: "dropdown-divider"} []]

-- | surgar for target : "blank"
--data LiNav_ = LiNav_ { title  :: String
--                     , href   :: String
--                     , icon   :: String
--                     , text   :: String
--                     , target :: String
--                     }

data LiNav = LiNav { title :: String
                   , href  :: String
                   , icon  :: String
                   , text  :: String
                   }

liNav :: LiNav -> R.Element
liNav (LiNav { title : title'
             , href  : href'
             , icon  : icon'
             , text  : text'
             }
      ) = H.li { className: "dropdown-item" } [
            H.a { tabIndex: (-1)
                , target: "blank"
                , title: title'
                , href: href'
                } [ H.span { className: icon' } []
                  , H.text $ " " <> text'
                  ]
            ]

------------------------------------------------------

handedChooser :: R2.Leaf ()
handedChooser = R2.leaf handedChooserCpt
handedChooserCpt :: R.Component ()
handedChooserCpt = here.component "handedChooser" cpt where
  cpt _ _ = do
    -- | States
    -- |
    { handed
    } <- AppStore.use

    handed' <- R2.useLive' handed

    -- | Computed
    -- |
    let
      handedClass LeftHanded = "fa fa-hand-o-left"
      handedClass RightHanded = "fa fa-hand-o-right"

    -- | Behaviors
    -- |
    let
      onClick :: Unit -> Effect Unit
      onClick _ = flip T.modify_ handed case _ of
        LeftHanded  -> RightHanded
        RightHanded -> LeftHanded

    -- | Render
    -- |
    pure $

      H.a
      { className: "nav-link navbar-text" }
      [
        H.span
        { className: handedClass handed'
        , on: { click: onClick }
        } []
      ]
