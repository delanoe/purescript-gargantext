module Gargantext.Components.App where

import Data.Array (fromFoldable, reverse)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..), maybe')
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude

import Gargantext.Config (publicBackend)
import Gargantext.Components.Forest (forest)
import Gargantext.Components.GraphExplorer (explorerLayout)
import Gargantext.Components.Lang (LandingLang(..))
import Gargantext.Components.Login (login)
import Gargantext.Components.Nodes.Annuaire (annuaireLayout)
import Gargantext.Components.Nodes.Annuaire.User.Contacts (annuaireUserLayout, userLayout)
import Gargantext.Components.Nodes.Corpus (corpusLayout)
import Gargantext.Components.Nodes.Corpus.Dashboard (dashboardLayout)
import Gargantext.Components.Nodes.Corpus.Document (documentLayout)
import Gargantext.Components.Nodes.File (fileLayout)
import Gargantext.Components.Nodes.Frame  (frameLayout)
import Gargantext.Components.Nodes.Home (homeLayout)
import Gargantext.Components.Nodes.Lists (listsLayout)
import Gargantext.Components.Nodes.Texts (textsLayout)
import Gargantext.Config (defaultFrontends, defaultBackends)
import Gargantext.Ends (Frontends, Backend)
import Gargantext.Hooks.Router (useHashRouter)
import Gargantext.License (license)
import Gargantext.Router (router)
import Gargantext.Routes (AppRoute(..))
import Gargantext.Sessions (Sessions, useSessions)
import Gargantext.Sessions as Sessions
import Gargantext.Types as GT
import Gargantext.Utils.Reactix as R2

thisModule :: String
thisModule = "Gargantext.Components.App"

-- TODO (what does this mean?)
-- tree changes endConfig state => trigger endConfig change in outerLayout, layoutFooter etc

app :: {} -> R.Element
app props = R.createElement appCpt props []

appCpt :: R.Component ()
appCpt = R.hooksComponentWithModule thisModule "app" cpt where
  frontends = defaultFrontends
  cpt _ _ = do
    sessions   <- useSessions
    route      <- useHashRouter router Home

    showLogin  <- R.useState' false
    backend    <- R.useState' Nothing

    showCorpus <- R.useState' false

    treeReload <- R.useState' 0

    handed <- R.useState' GT.RightHanded

    let backends          = fromFoldable defaultBackends
    let ff f session      = R.fragment [ f session, footer { session } ]
    let forested child    = forestLayout { child
                                         , frontends
                                         , handed
                                         , reload: treeReload
                                         , route:  fst route
                                         , sessions: fst sessions
                                         , showLogin: snd showLogin
                                         , backend
                                         }
    let defaultView _ = forested $ homeLayout { backend
                                              , lang: LL_EN
                                              , publicBackend
                                              , sessions
                                              , visible: showLogin
                                              }
    let mCurrentRoute     = fst route
    let withSession sid f = maybe' defaultView (ff f) (Sessions.lookup sid (fst sessions))

    let sessionUpdate s = snd sessions $ Sessions.Update s

    pure $ case fst showLogin of
      true -> forested $ login { backend, backends, sessions, visible: showLogin }
      false ->
        case fst route of
          Annuaire sid nodeId        -> withSession sid $ \session -> forested $ annuaireLayout { frontends, nodeId, session }
          ContactPage sid aId nodeId                -> withSession sid $ \session -> forested $ annuaireUserLayout { annuaireId: aId, frontends, nodeId, session }
          Corpus sid nodeId        -> withSession sid $ \session -> forested $ corpusLayout { nodeId, session }
          CorpusDocument sid corpusId listId nodeId -> withSession sid $ \session -> forested $ documentLayout { nodeId, listId, session, corpusId: Just corpusId }
          Dashboard sid nodeId       -> withSession sid $ \session -> forested $ dashboardLayout { nodeId, session }
          Document sid listId nodeId ->
            withSession sid $
              \session -> forested $ documentLayout { nodeId, listId, session, corpusId: Nothing }
          Folder sid nodeId -> withSession sid $ \session -> forested $ corpusLayout { nodeId, session }
          FolderPrivate sid nodeId -> withSession sid $ \session -> forested $ corpusLayout { nodeId, session }
          FolderPublic sid nodeId  -> withSession sid $ \session -> forested $ corpusLayout { nodeId, session }
          FolderShared sid nodeId  -> withSession sid $ \session -> forested $ corpusLayout { nodeId, session }
          Home  -> forested $ homeLayout { backend, lang:LL_EN, publicBackend, sessions, visible: showLogin }
          Lists sid nodeId         -> withSession sid $ \session -> forested $ listsLayout { nodeId, session, sessionUpdate }
          Login -> login { backend, backends, sessions, visible: showLogin }
          PGraphExplorer sid graphId ->
            withSession sid $
              \session ->
                simpleLayout handed $
                  explorerLayout { backend
                                 , frontends
                                 , graphId
                                 , handed: fst handed
                                 , mCurrentRoute
                                 , session
                                 , sessions: (fst sessions)
                                 , showLogin
                                 --, treeReload
                                 }
          RouteFile sid nodeId -> withSession sid $ \session -> forested $ fileLayout { nodeId, session }
          RouteFrameCalc  sid nodeId -> withSession sid $ \session -> forested $ frameLayout { nodeId, session }
          RouteFrameWrite sid nodeId -> withSession sid $ \session -> forested $ frameLayout { nodeId, session }
          Team sid nodeId  -> withSession sid $ \session -> forested $ corpusLayout { nodeId, session }
          Texts sid nodeId         -> withSession sid $ \session -> forested $ textsLayout { frontends, nodeId, session, sessionUpdate }
          UserPage sid nodeId        -> withSession sid $ \session -> forested $ userLayout { frontends, nodeId, session }

type ForestLayoutProps =
  ( backend   :: R.State (Maybe Backend)
  , child     :: R.Element
  , frontends :: Frontends
  , handed    :: R.State GT.Handed
  , reload    :: R.State Int
  , route     :: AppRoute
  , sessions  :: Sessions
  , showLogin :: R.Setter Boolean
  )

forestLayout :: Record ForestLayoutProps -> R.Element
forestLayout props = R.createElement forestLayoutCpt props []

forestLayoutCpt :: R.Component ForestLayoutProps
forestLayoutCpt = R.hooksComponentWithModule thisModule "forestLayout" cpt
  where
    cpt props@{ handed } _ = do
      pure $ R.fragment [ topBar { handed }, forestLayoutMain props ]

forestLayoutMain :: Record ForestLayoutProps -> R.Element
forestLayoutMain props = R.createElement forestLayoutMainCpt props []

forestLayoutMainCpt :: R.Component ForestLayoutProps
forestLayoutMainCpt = R.hooksComponentWithModule thisModule "forestLayoutMain" cpt
  where
    cpt { child, frontends, handed, reload, route, sessions, showLogin, backend} _ = do
      let ordering =
            case fst handed of
              GT.LeftHanded  -> reverse
              GT.RightHanded -> identity

      pure $ R2.row $ ordering [
        H.div { className: "col-md-2", style: { paddingTop: "60px" } }
            [ forest { frontends, handed: fst handed, reload, route, sessions, showLogin, backend} ]
      , mainPage child
      ]

-- Simple layout does not accommodate the tree
simpleLayout :: R.State GT.Handed -> R.Element -> R.Element
simpleLayout handed child = H.div { className: "simple-layout" } [
    topBar { handed }
  , child
  , license
  ]

mainPage :: R.Element -> R.Element
mainPage child =
  H.div {className: "col-md-10"}
  [ H.div {id: "page-wrapper"}
    [ H.div {className: "container-fluid"} [ child ] ] ]

type TopBarProps = (
  handed :: R.State GT.Handed
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
            navHanded  = if fst handed == GT.LeftHanded then " navbar-right" else ""
            sortHanded = if fst handed == GT.LeftHanded then reverse else reverse -- identity
            -- SB.searchBar {session, databases: allDatabases}

type HandedChooserProps = (
  handed :: R.State GT.Handed
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

    handedClass (GT.LeftHanded  /\ _) = "fa fa-hand-o-left"
    handedClass (GT.RightHanded /\ _) = "fa fa-hand-o-right"

    onClick (_ /\ setHanded) = setHanded $ \h -> case h of
      GT.LeftHanded  -> GT.RightHanded
      GT.RightHanded -> GT.LeftHanded

logo :: GT.Handed -> R.Element
logo handed =
  H.a { className, href: "#/" }
  [ H.img { src, title, width: "30", height: "28" }
  ]
  where
    className = "navbar-brand logoSmall" <> navHanded
    src       = "images/logoSmall.png"
    title     = "Back to home."
    navHanded = if handed == GT.LeftHanded then " navbar-right" else ""

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

---------------------------------------------------------------------------
type FooterProps =
  (
    session :: Sessions.Session
  )

footer :: Record FooterProps -> R.Element
footer props = R.createElement footerCpt props []

footerCpt :: R.Component FooterProps
footerCpt = R.hooksComponentWithModule thisModule "footer" cpt
  where
    cpt { session } _ = do
      pure $ H.div 
              { className: "container" }
              [ H.hr {}
              , H.footer {} []
              ]

