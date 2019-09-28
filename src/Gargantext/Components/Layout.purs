module Gargantext.Components.Layout where

import Data.Foldable (fold, intercalate)
import Data.Lens (over)
import Data.Maybe (Maybe(..), maybe')
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested((/\))
import DOM.Simple.Console (log2)
import Effect (Effect)
import Effect.Aff (launchAff)
import React.DOM (button, div, text)
import React.DOM.Props (_id, className, onClick, role, style)
import Reactix as R
import Reactix.DOM.HTML as H
-- import Unsafe.Coerce (unsafeCoerce)

import Gargantext.BootstrapNative (createDropdown)
import Gargantext.Prelude
import Gargantext.Components.Data.Lang (Lang(..))
import Gargantext.Components.EndsChooser as EndsChooser
import Gargantext.Components.EndsSummary (endsSummary)
import Gargantext.Components.GraphExplorer (explorerLayout)
import Gargantext.Components.Login.Types (AuthData(..))
import Gargantext.Components.Login (Auths, getCurrentAuth, setAuths, login)
import Gargantext.Components.Search.SearchBar as SB
import Gargantext.Components.Tree  as Tree
import Gargantext.Config (Ends, defaultEnds, backendKey)
import Gargantext.Components.Folder (folder)
import Gargantext.Pages.Annuaire (annuaireLayout)
import Gargantext.Pages.Annuaire.User.Contacts (userLayout)
import Gargantext.Pages.Corpus (corpusLayout)
import Gargantext.Pages.Corpus.Document (documentLayout)
import Gargantext.Pages.Corpus.Dashboard (dashboardLayout)
import Gargantext.Pages.Lists (listsLayout)
import Gargantext.Pages.Texts (textsLayout)
import Gargantext.Pages.Home (layoutLanding)
import Gargantext.Router (Routes(..), routing, useHashRouter)
import Gargantext.Utils.Reactix as R2
import Gargantext.Global (Global, defaultGlobal)

-- TODO (what does this mean?)
-- tree changes endConfig state => trigger endConfig change in outerLayout, layoutFooter etc

type State =
  ( ends          :: R.State Ends
  , auths         :: R.State Auths
  , route         :: R.State Routes
  , showLogin     :: R.State Boolean
  , showCorpus    :: R.State Boolean
  , showTree      :: R.State Boolean )

layout :: _ -> R.Element
layout _ = R.createElement layoutCpt {} []

layoutCpt :: R.Component ( )
layoutCpt = R.hooksComponent "Layout" cpt
  where
    cpt _ _ = do
      state <- usePagesState
      pure $ pages state

pages :: Record State -> R.Element
pages props = R.createElement pagesCpt props []

pagesCpt :: R.Component State
pagesCpt = R.staticComponent "Pages" cpt
  where
    cpt state@{ends, route, showLogin, showCorpus, showTree} _ = do
      case (fst route) of
        Home -> tree $ layoutLanding EN
        Login -> login { ends: (fst ends), setVisible: (snd showLogin) }
        Folder _ -> tree $ folder {}
        Corpus nodeId -> tree $ corpusLayout {nodeId, ends: fst ends}
        Texts nodeId -> tree $ textsLayout {nodeId, ends: fst ends}
        Lists nodeId -> tree $ listsLayout {nodeId, ends: fst ends}
        Dashboard -> tree $ dashboardLayout {}
        Annuaire annuaireId -> tree $ annuaireLayout { annuaireId, ends: fst ends }
        UserPage nodeId -> tree $ userLayout { nodeId, ends: fst ends }
        ContactPage nodeId -> tree $ userLayout { nodeId, ends: fst ends }
        CorpusDocument corpusId listId nodeId ->
          tree $ documentLayout { nodeId, listId, corpusId: Just corpusId, ends: fst ends }
        Document listId nodeId ->
          tree $ documentLayout { nodeId, listId, corpusId: Nothing, ends: fst ends }
        PGraphExplorer graphId ->
          simpleLayout state $ explorerLayout {graphId, mCurrentRoute, treeId: Nothing, ends: fst ends}
      where
        mCurrentRoute = Just $ fst route
        tree = treeLayout state

usePagesState :: R.Hooks (Record State)
usePagesState = do
  ends <- R.useState' defaultEnds
  auths <- R.useState' Map.empty
  route <- useHashRouter routing Home
  showLogin <- R.useState' false
  showCorpus <- R.useState' false
  showTree <- R.useState' false
  pure $ {ends, auths, route, showLogin, showCorpus, showTree}

treeLayout :: Record State -> R.Element -> R.Element
treeLayout state@{ends, auths, route, showTree} child =
  R.fragment [ searchBar state, row layout', footer {} ]
  where
    backendAuth = getCurrentAuth (fst ends) (fst auths)
    layout' = maybe' (\_ -> mainPage false child) (withTree <<< unwrap) backendAuth
    withTree {tree_id} =
      R.fragment
      [ H.div {className: "col-md-2", style: {paddingTop: "60px"}}
        [ Tree.treeView { root: tree_id, mCurrentRoute: Just (fst route), ends: (fst ends) } ]
      , mainPage true child ]
    row child' = H.div {className: "row"} [child']

-- Simple layout does not accommodate the tree
simpleLayout :: Record State -> R.Element -> R.Element
simpleLayout state child = R.fragment [ searchBar state, child, footer {}]

mainPage :: Boolean -> R.Element -> R.Element
mainPage showTree child =
  H.div {className}
  [ H.div {id: "page-wrapper"}
    [ H.div {className: "container-fluid"} [ child ] ] ]
  where
    className
      | showTree = "col-md-10"
      | otherwise = "col-md-12"

searchBar :: Record State -> R.Element
searchBar state@{ends} =
  H.div { id: "dafixedtop", role: "navigation"
        , className: "navbar navbar-inverse navbar-fixed-top" }
  [ H.div { className: "container-fluid" }
    [ H.div { className: "navbar-inner" }
      [ logo
      , H.div { className: "collapse navbar-collapse" }
        [ divDropdownLeft
        , SB.searchBar (SB.defaultProps (fst ends))
        , divDropdownRight state ] ] ] ]

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


loginLinks :: Record State -> R.Element
loginLinks state@{ends, auths, showLogin} =
  case getCurrentAuth (fst ends) (fst auths) of
    Nothing -> loginLink
    Just _  -> logoutLink
  where
    loginLink =
      H.a { aria: {hidden : true}
          , className: "glyphicon glyphicon-log-in"
          , on: {click:  \e -> (snd showLogin) (const true)}
          , style: {color:"white"}
          , title: "Log in and save your time"
          -- TODO hover: bold
          }
          [H.text " Login / Signup"]
    -- TODO dropdown to logout
    logoutLink =
      H.a { aria: {hidden : true}
          , className: "glyphicon glyphicon-log-out"
          , on: {click: \e -> logout state}
          , style: {color:"white"}
          , title: "Log out" -- TODO
          -- TODO hover: bold
          }
          [H.text " Logout"]

logout :: Record State -> Effect Unit
logout {ends, auths} = (snd auths) (const auths2) *> setAuths (Just auths2)
  where
    key = backendKey (fst ends).backend
    auths2 = Map.delete key (fst auths)

divDropdownRight :: Record State -> R.Element
divDropdownRight props = R.createElement divDropdownRightCpt props []

divDropdownRightCpt :: R.Component State
divDropdownRightCpt = R.staticComponent "G.C.Layout.divDropdownRight" cpt
  where
    cpt state@{ends} _ =
      H.ul {className: "nav navbar-nav pull-right"}
      [ endsSummary (fst ends), loginLinks state ]

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

