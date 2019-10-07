module Gargantext.Components.App where

import Prelude
import Data.Array (fromFoldable)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.Tuple (fst, snd)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Components.Data.Lang (Lang(..))
import Gargantext.Components.Forest (forest)
import Gargantext.Components.GraphExplorer (explorerLayout)
import Gargantext.Components.Login (login)
import Gargantext.Components.Search.SearchBar as SB
import Gargantext.Components.Search.Types (allDatabases)
import Gargantext.Config (defaultFrontends, defaultBackends)
import Gargantext.Components.Folder (folder)
import Gargantext.Ends (Frontends)
import Gargantext.Pages.Annuaire (annuaireLayout)
import Gargantext.Pages.Annuaire.User.Contacts (userLayout)
import Gargantext.Pages.Corpus (corpusLayout)
import Gargantext.Pages.Corpus.Document (documentLayout)
import Gargantext.Pages.Corpus.Dashboard (dashboardLayout)
import Gargantext.Pages.Lists (listsLayout)
import Gargantext.Pages.Texts (textsLayout)
import Gargantext.Pages.Home (homeLayout)
import Gargantext.Router (router)
import Gargantext.Routes (AppRoute(..))
import Gargantext.Hooks.Router (useHashRouter)
import Gargantext.Utils.Reactix as R2
import Gargantext.Sessions (Sessions, useSessions, unSessions)

-- TODO (what does this mean?)
-- tree changes endConfig state => trigger endConfig change in outerLayout, layoutFooter etc

app :: {} -> R.Element
app props = R.createElement appCpt props []

appCpt :: R.Component ()
appCpt = R.hooksComponent "G.C.App.app" cpt where
  frontends = defaultFrontends
  cpt _ _ = do
    sessions   <- useSessions
    route      <- useHashRouter router Home
    
    showLogin  <- R.useState' false
    showCorpus <- R.useState' false
    
    let tree          = forestLayout frontends (fst sessions) (fst route) (snd showLogin)
    let mCurrentRoute = fst route
    let backends      = fromFoldable defaultBackends
    pure $ case fst showLogin of
      true -> tree $ login { sessions, backends, visible: showLogin }
      false ->
        case unSessions (fst sessions) of
          Nothing -> tree $ homeLayout EN
          Just session ->
            case (fst route) of
              Home     -> tree $ homeLayout EN
              Login    -> login { sessions, backends, visible: showLogin }
              Folder _ -> tree $ folder {}
              Corpus nodeId -> tree $ corpusLayout { nodeId }
              Texts  nodeId -> tree $ textsLayout { nodeId, session }
              Lists  nodeId -> tree $ listsLayout { nodeId, session }
              Dashboard _nodeId -> tree $ dashboardLayout {}
              Annuaire    nodeId -> tree $ annuaireLayout { nodeId, session }
              UserPage    nodeId -> tree $ userLayout { nodeId, session }
              ContactPage nodeId -> tree $ userLayout { nodeId, session }
              CorpusDocument corpusId listId nodeId ->
                tree $ documentLayout { nodeId, listId, session, corpusId: Just corpusId }
              Document listId nodeId ->
                tree $ documentLayout { nodeId, listId, session, corpusId: Nothing }
              PGraphExplorer graphId ->
                simpleLayout (fst sessions) $
                  explorerLayout { graphId, mCurrentRoute, session, sessions, treeId: Nothing, frontends}

forestLayout :: Frontends -> Sessions -> AppRoute -> R2.Setter Boolean -> R.Element -> R.Element
forestLayout frontends sessions route showLogin child =
  R.fragment [ topBar sessions, row main, footer {} ]
  where
    row child' = H.div {className: "row"} [child']
    main =
      R.fragment
      [ H.div {className: "col-md-2", style: {paddingTop: "60px"}}
              [ forest {sessions, route, frontends, showLogin} ]
      , mainPage child
      ]

-- Simple layout does not accommodate the tree
simpleLayout :: R.Element -> R.Element
simpleLayout child = R.fragment [ topBar {}, child, footer {}]

mainPage :: R.Element -> R.Element
mainPage child =
  H.div {className: "col-md-10"}
  [ H.div {id: "page-wrapper"}
    [ H.div {className: "container-fluid"} [ child ] ] ]

topBar :: {} -> R.Element
topBar _ =
  H.div { id: "dafixedtop", role: "navigation"
        , className: "navbar navbar-inverse navbar-fixed-top" }
  [ H.div { className: "container-fluid" }
    [ H.div { className: "navbar-inner" }
      [ logo
      , H.div { className: "collapse navbar-collapse" }
        [ divDropdownLeft ] ] ] ]
      -- SB.searchBar {session, databases: allDatabases}

logo :: R.Element
logo =
  H.a { className, href: "#/" }
  [ H.img { src, title, width: "30", height: "28" } ]
  where
    className = "navbar-brand logoSmall"
    src = "images/logoSmall.png"
    title = "Back to home."

divDropdownLeft :: R.Element
divDropdownLeft =
  divDropdownLeft' $
    LiNav { title : "About Gargantext"
          , href  : "#"
          , icon  : "glyphicon glyphicon-info-sign"
          , text  : "Info" }

divDropdownLeft' :: LiNav -> R.Element
divDropdownLeft' mb =
  H.ul {className: "nav navbar-nav"}
  [ H.ul {className: "nav navbar-nav pull-left"}
    [ H.li {className: "dropdown"} [ menuButton mb, menuElements' ] ] ]

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
            , icon  : "glyphicon glyphicon-book"
            , text  : "Documentation"
            }
    , LiNav { title : "Report bug here"
            , href  : "https://www.iscpif.fr/gargantext/feedback-and-bug-reports/"
            , icon  : "glyphicon glyphicon-bullhorn"
            , text  : "Feedback"
            }
    ]
    , -----------------------------------------------------------
    [ LiNav { title : "Interactive chat"
            , href  : "https://chat.iscpif.fr/channel/gargantext"
            , icon  : "fab fa-rocketchat"
            , text  : "Chat"
            }
    , LiNav { title : "Asynchronous discussions"
            , href  : "https://discourse.iscpif.fr/c/gargantext"
            , icon  : "fab fa-discourse"
            , text  : "Forum"
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

footer :: {} -> R.Element
footer props = R.createElement footerCpt props []

footerCpt :: R.Component ()
footerCpt = R.staticComponent "G.C.Layout.footer" cpt
  where
    cpt _ _ =
      H.div { className: "container" }
      [ H.hr {}
      , H.footer {}
        [ H.p {}
          [ H.text "Gargantext "
          , H.span {className: "glyphicon glyphicon-registration-mark"} []
          , H.text ", version 4.0"
          , H.a { href: "http://www.cnrs.fr"
                , target: "blank"
                , title: "Project hosted by CNRS."
                }
            [ H.text ", Copyrights "
            , H.span { className: "glyphicon glyphicon-copyright-mark" } []
            , H.text " CNRS 2017-Present"
            ]
          , H.a { href: "http://gitlab.iscpif.fr/humanities/gargantext/blob/stable/LICENSE"
                , target: "blank"
                , title: "Legal instructions of the project."
                }
            [ H.text ", Licences aGPLV3 and CECILL variant Affero compliant" ]
          , H.text "."
          ]]
        ]

