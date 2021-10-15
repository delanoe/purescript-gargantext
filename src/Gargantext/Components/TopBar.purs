module Gargantext.Components.TopBar where

import Gargantext.Prelude

import Data.Foldable (intercalate)
import Gargantext.Components.App.Data (Boxes)
import Gargantext.Components.GraphExplorer.ToggleButton as Toggle
import Gargantext.Components.Themes (themeSwitcher, allThemes)
import Gargantext.Types (Handed(..), reverseHanded)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.TopBar"

type TopBarProps =
  ( boxes :: Boxes )

topBar :: R2.Component TopBarProps
topBar = R.createElement topBarCpt
topBarCpt :: R.Component TopBarProps
topBarCpt = here.component "topBar" cpt
  where
    cpt { boxes: { handed, showTree, theme } } children = do
      handed' <- T.useLive T.unequal handed

      pure $ H.div { className: "navbar navbar-expand-lg navbar-dark bg-dark"
                   , id: "dafixedtop"
                   , role: "navigation"
                   }
        [ H.div { className: "container-fluid" } $ reverseHanded handed'
          [
            -- NOTE: first (and only) entry in the sorted array should have the "ml-auto class"
            -- https://stackoverflow.com/questions/19733447/bootstrap-navbar-with-left-center-or-right-aligned-items
            -- In practice: only apply "ml-auto" to the last element of this list, if handed == LeftHanded
            logo
          , H.div { className: "collapse navbar-collapse" }
            [ H.ul { className: "navbar-nav " <> if handed' == LeftHanded then "ml-auto" else "" } $ reverseHanded handed'
              ([ divDropdownLeft {} []
               , handButton
               , smiley
               , H.li { className: "nav-item" } [ themeSwitcher { theme
                                                                , themes: allThemes } [] ]
               , Toggle.treeToggleButton { state: showTree } []
               ] <> children)
            ]
          ]
        ]
          where
            handButton = H.li { title: "If you are Left Handed you can change\n"
                                <> "the interface by clicking on me. Click\n"
                                <> "again to come back to previous state."
                              , className: "nav-item"
                              } [handedChooser { handed } []]

            smiley = H.li { title: "Hello! Looking for the tree ?\n"
                                <> "Just watch on the other side!\n"
                                <> "Click on the hand again to see it back."
                          , className : "nav-item"
                          }
                          [ H.a { className: "nav-link" } [H.span {className: "fa fa-question-circle-o"} [] ]]

                        {-, H.ul { title: "Dark Mode soon here"
                                , className : "nav navbar-nav"
                                } [ H.li {} [ H.a {} [ H.span {className : "fa fa-moon"}[]
                                                          ]
                                                ]
                                      ]
                              -}

            -- SB.searchBar {session, databases: allDatabases}

logo :: R.Element
logo =
  H.a { className, href: "#/" } [
    H.img { src, title, width: "30", height: "28" }
  ]
  where
    className = "navbar-brand logoSmall"
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
      [ LiNav { title : "Code documentation"
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
      [ LiNav { title : "More about us (you)"
              , href  : "https://iscpif.fr"
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


type HandedChooserProps = (
  handed :: T.Box Handed
  )

handedChooser :: R2.Component HandedChooserProps
handedChooser = R.createElement handedChooserCpt

handedChooserCpt :: R.Component HandedChooserProps
handedChooserCpt = here.component "handedChooser" cpt
  where
    cpt { handed } _ = do
      handed' <- T.useLive T.unequal handed

      pure $ H.a { className: "nav-link" } [
        H.span { className: handedClass handed'
               , on: { click: onClick handed } } []
        ]

    handedClass LeftHanded = "fa fa-hand-o-left"
    handedClass RightHanded = "fa fa-hand-o-right"

    onClick handed = T.modify_ (\h -> case h of
      LeftHanded  -> RightHanded
      RightHanded -> LeftHanded) handed
