module Gargantext.Components.App where

import Prelude

import Data.Array (fromFoldable)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..), maybe')
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.License (license)
import Gargantext.Components.Lang (LandingLang(..))
import Gargantext.Components.Forest (forest)
import Gargantext.Components.GraphExplorer (explorerLayout)
import Gargantext.Components.Login (login)
import Gargantext.Components.Nodes.Annuaire (annuaireLayout)
import Gargantext.Components.Nodes.Annuaire.User.Contacts (annuaireUserLayout, userLayout)
import Gargantext.Components.Nodes.Corpus (corpusLayout)
import Gargantext.Components.Nodes.Frame  (frameLayout)
import Gargantext.Components.Nodes.Corpus.Dashboard (dashboardLayout)
import Gargantext.Components.Nodes.Corpus.Document (documentLayout)
import Gargantext.Components.Nodes.Home (homeLayout)
import Gargantext.Components.Nodes.Lists (listsLayout)
import Gargantext.Components.Nodes.Texts (textsLayout)
import Gargantext.Config (defaultFrontends, defaultBackends)
import Gargantext.Ends (Frontends)
import Gargantext.Hooks.Router (useHashRouter)
import Gargantext.Router (router)
import Gargantext.Routes (AppRoute(..))
import Gargantext.Sessions (Sessions, useSessions)
import Gargantext.Sessions as Sessions
import Gargantext.Utils.Reactix as R2
import Gargantext.Version as GV

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

    treeReload <- R.useState' 0

    let forested child = forestLayout { child
                                      , frontends
                                      , reload: treeReload
                                      , route:  fst route
                                      , sessions: fst sessions
                                      , showLogin: snd showLogin }
    let mCurrentRoute = fst route
    let backends      = fromFoldable defaultBackends
    let ff f session = R.fragment [ f session, footer { session } ]
    let withSession sid f =
          maybe' (const $ forested $ homeLayout LL_EN) (ff f) $ Sessions.lookup sid (fst sessions)

    pure $ case fst showLogin of
      true -> forested $ login { sessions, backends, visible: showLogin }
      false ->
        case fst route of
          Home  -> forested $ homeLayout LL_EN
          Login -> login { sessions, backends, visible: showLogin }
          --Folder sid _      -> withSession sid $ \_ -> forested (folder {})
          Folder sid nodeId        -> withSession sid $ \session -> forested $ corpusLayout { key: show nodeId, nodeId, session }
          FolderPrivate sid nodeId -> withSession sid $ \session -> forested $ corpusLayout { key: show nodeId, nodeId, session }
          FolderPublic sid nodeId  -> withSession sid $ \session -> forested $ corpusLayout { key: show nodeId, nodeId, session }
          FolderShared sid nodeId  -> withSession sid $ \session -> forested $ corpusLayout { key: show nodeId, nodeId, session }
          Team sid nodeId  -> withSession sid $ \session -> forested $ corpusLayout { key: show nodeId, nodeId, session }
          RouteFrameWrite sid nodeId -> withSession sid $ \session -> forested $ frameLayout { key: show nodeId, nodeId, session }
          RouteFrameCalc  sid nodeId -> withSession sid $ \session -> forested $ frameLayout { key: show nodeId, nodeId, session }
          Corpus sid nodeId        -> withSession sid $ \session -> forested $ corpusLayout { key: show nodeId, nodeId, session }
          Texts sid nodeId         -> withSession sid $ \session -> forested $ textsLayout { nodeId, session, frontends }
          Lists sid nodeId         -> withSession sid $ \session -> forested $ listsLayout { nodeId, session }
          Dashboard sid nodeId       -> withSession sid $ \session -> forested $ dashboardLayout { nodeId, session }
          Annuaire sid nodeId        -> withSession sid $ \session -> forested $ annuaireLayout { frontends, nodeId, session }
          UserPage sid nodeId        -> withSession sid $ \session -> forested $ userLayout { frontends, nodeId, session }
          ContactPage sid aId nodeId                -> withSession sid $ \session -> forested $ annuaireUserLayout { annuaireId: aId, frontends, nodeId, session }
          CorpusDocument sid corpusId listId nodeId -> withSession sid $ \session -> forested $ documentLayout { nodeId, listId, session, corpusId: Just corpusId }
          Document sid listId nodeId ->
            withSession sid $
              \session -> forested $ documentLayout { nodeId, listId, session, corpusId: Nothing }
          PGraphExplorer sid graphId ->
            withSession sid $
              \session ->
                simpleLayout $
                  explorerLayout { frontends
                                 , graphId
                                 , mCurrentRoute
                                 , session
                                 , sessions: (fst sessions)
                                 , showLogin
                                 , treeReload }

type ForestLayoutProps =
  ( child     :: R.Element
  , frontends :: Frontends
  , reload    :: R.State Int
  , route     :: AppRoute
  , sessions  :: Sessions
  , showLogin :: R2.Setter Boolean
  )

forestLayout :: Record ForestLayoutProps -> R.Element
forestLayout { child, frontends, reload, route, sessions, showLogin } = do
  R.fragment [ topBar {}, R2.row [main] ]
  where
    main =
      R.fragment
      [ H.div {className: "col-md-2", style: {paddingTop: "60px"}}
              [ forest { frontends, reload, route, sessions, showLogin } ]
      , mainPage child
      ]

-- Simple layout does not accommodate the tree
simpleLayout :: R.Element -> R.Element
simpleLayout child = R.fragment [ topBar {}, child, license]

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

---------------------------------------------------------------------------
-- | TODO put Version in the Tree/Root node

type VersionProps =
  (
    session :: Sessions.Session
  )

version :: Record VersionProps -> R.Element
version props = R.createElement versionCpt props []

versionCpt :: R.Component VersionProps
versionCpt = R.hooksComponent "G.C.A.version" cpt
  where
    cpt { session } _ = do
      (ver /\ setVer) <- R.useState' "No Backend Version"

      R.useEffect' $ do
        launchAff_ $ do
          v <- GV.getBackendVersion session
          liftEffect $ setVer $ const v

      pure $ H.div { className: "row" }
                   [ H.div { className: versionCheck GV.version ver}
                           [ H.h4 {} [H.text $ versionMessage GV.version ver]
                           , H.div { className: "container" } [showVersions GV.version ver]
                           ]
                   ]
          where
            versionCheck v1 v2 = case v1 == v2 of
                false -> "col alert alert-danger"
                true  -> "col alert alert-success"
            versionMessage v1 v2 = case v1 == v2 of
                false -> "Versions do not match"
                true  -> "Versions are up to date"

    showVersions frontendVer backendVer =
      H.div { className: "row" }
            [ H.h5 {} [ H.text $ "Frontend version: " <> frontendVer ]
            , H.h5 {} [ H.text $ "Backend  version: "  <> backendVer ]
            ]

footer :: Record VersionProps -> R.Element
footer props = R.createElement footerCpt props []

footerCpt :: R.Component VersionProps
footerCpt = R.hooksComponent "G.C.A.footer" cpt
  where
    cpt { session } _ = do
      pure $ H.div 
              { className: "container" }
              [ H.hr {}
              , H.footer {} [ version { session }
                            , license
                            ]
              ]

