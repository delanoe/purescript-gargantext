module Gargantext.Components.TopBar where

import Data.Array (reverse)
import Data.Foldable (intercalate)
import Data.Tuple (fst)
import Data.Tuple.Nested ((/\))
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude

import Gargantext.Types (Handed(..))

thisModule :: String
thisModule = "Gargantext.Components.TopBar"

type TopBarProps = (
  handed :: R.State Handed
  )

topBar :: Record TopBarProps -> R.Element
topBar props = R.createElement topBarCpt props []

topBarCpt :: R.Component TopBarProps
topBarCpt = R.hooksComponentWithModule thisModule "topBar" cpt
  where
    cpt { handed } _ = do
      pure $ H.div { id: "dafixedtop"
                   , role: "navigation"
                   , className: "navbar navbar-inverse navbar-fixed-top" }
        [ H.div { className: "container-fluid" }
            [ H.div { className: "navbar-inner" }
              [ logo (fst handed)
              , H.div { className: "collapse navbar-collapse"  <> navHanded}
                      $ sortHanded
                      [ H.ul { className: "nav navbar-nav" <> navHanded} [divDropdownLeft]
                      , H.ul { title: "If you are Left Handed you can change "
                                    <> "the interface by clicking on me. Click "
                                    <> "again to come back to previous state."
                             , className: "nav navbar-nav" <> navHanded
                             } [handedChooser { handed }]
                      , H.ul { className: "nav navbar-nav" <> navHanded} []
                      {-, H.ul { title: "Dark Mode soon here"
                             , className : "nav navbar-nav"
                             } [ H.li {} [ H.a {} [ H.span {className : "fa fa-moon"}[]
                                                  ]
                                         ]
                               ]
                      -}
                      ]
              ]
            ]
        ]
          where
            navHanded  = if fst handed == LeftHanded then " navbar-right" else ""
            sortHanded = if fst handed == LeftHanded then reverse else reverse -- identity
            -- SB.searchBar {session, databases: allDatabases}


logo :: Handed -> R.Element
logo handed =
  H.a { className, href: "#/" }
  [ H.img { src, title, width: "30", height: "28" }
  ]
  where
    className = "navbar-brand logoSmall" <> navHanded
    src       = "images/logoSmall.png"
    title     = "Back to home."
    navHanded = if handed == LeftHanded then " navbar-right" else ""


divDropdownLeft :: R.Element
divDropdownLeft =
  divDropdownLeft' $
    LiNav { title : "About Gargantext"
          , href  : "#"
          , icon  : "fa fa-info-circle"
          , text  : "Info" }

divDropdownLeft' :: LiNav -> R.Element
divDropdownLeft' mb =
  H.li {className: "dropdown"} [ menuButton mb, menuElements' ]


menuButton :: LiNav -> R.Element
menuButton (LiNav { title, href, icon, text } ) =
  H.a { className: "dropdown-toggle navbar-text"
      , data: {toggle: "dropdown"}
      , href, title
      , role: "button" }
  [ H.span { aria: {hidden : true}, className: icon } []
  , H.text (" " <> text) ]

menuElements' :: R.Element
menuElements' = menuElements-- title, icon, text
  [ -- ===========================================================
    [ LiNav { title : "Quick start, tutorials and methodology"
            , href  : "https://iscpif.fr/gargantext/your-first-map/"
            , icon  : "fa fa-lightbulb-o"
            , text  : "Tutorials"
            }
    , LiNav { title : "Report bug here"
            , href  : "https://www.iscpif.fr/gargantext/feedback-and-bug-reports/"
            , icon  : "glyphicon glyphicon-bullhorn"
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
            , icon  : "glyphicon glyphicon-book"
            , text  : "Source Code Documentation"
            }
    , LiNav { title : "API documentation"
            , href  : "https://v4.gargantext.org/swagger-ui"
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
            , icon  : "glyphicon glyphicon-question-sign"
            , text  : "About"
            }
    ]
  ] -- ===========================================================

-- | Menu in the sidebar, syntactic sugar
menuElements :: Array (Array LiNav) -> R.Element
menuElements ns = dropDown $ intercalate divider $ map (map liNav) ns
  where
    dropDown :: Array R.Element -> R.Element
    dropDown = H.ul {className: "dropdown-menu"}

    divider :: Array R.Element
    divider = [H.li {className: "divider"} []]

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
      ) = H.li {} [ H.a { tabIndex: (-1)
                        , target: "blank"
                        , title: title'
                        , href: href'
                        } [ H.span { className: icon' } []
                          , H.text $ " " <> text'
                          ]
                  ]


type HandedChooserProps = (
  handed :: R.State Handed
  )

handedChooser :: Record HandedChooserProps -> R.Element
handedChooser props = R.createElement handedChooserCpt props []

handedChooserCpt :: R.Component HandedChooserProps
handedChooserCpt = R.hooksComponentWithModule thisModule "handedChooser" cpt
  where
    cpt { handed } _ = do
      pure $ H.li {} [
        H.a {} [
          H.span { className: handedClass handed
                 , on: { click: onClick handed } } []
          ]
        ]

    handedClass (LeftHanded  /\ _) = "fa fa-hand-o-left"
    handedClass (RightHanded /\ _) = "fa fa-hand-o-right"

    onClick (_ /\ setHanded) = setHanded $ \h -> case h of
      LeftHanded  -> RightHanded
      RightHanded -> LeftHanded
