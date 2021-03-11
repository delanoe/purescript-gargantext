module Gargantext.Components.Router (router) where

import Data.Array (fromFoldable)
import Data.Maybe (Maybe(..))
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Record.Extra as RE
import Toestand as T
import Unsafe.Coerce (unsafeCoerce)

import Gargantext.Prelude

import Gargantext.AsyncTasks as GAT
import Gargantext.Components.App.Data (Boxes)
import Gargantext.Components.Footer (footer)
import Gargantext.Components.Forest (forestLayout)
import Gargantext.Components.GraphExplorer (explorerLayoutLoader)
import Gargantext.Components.Lang (LandingLang(LL_EN))
import Gargantext.Components.Login (login)
import Gargantext.Components.Nodes.Annuaire (annuaireLayout)
import Gargantext.Components.Nodes.Annuaire.User (userLayout)
import Gargantext.Components.Nodes.Annuaire.User.Contact (contactLayout)
import Gargantext.Components.Nodes.Corpus (corpusLayout)
import Gargantext.Components.Nodes.Corpus.Dashboard (dashboardLayout)
import Gargantext.Components.Nodes.Corpus.Document (documentMainLayout)
import Gargantext.Components.Nodes.File (fileLayout)
import Gargantext.Components.Nodes.Frame (frameLayout)
import Gargantext.Components.Nodes.Home (homeLayout)
import Gargantext.Components.Nodes.Lists as Lists
import Gargantext.Components.Nodes.Texts as Texts
import Gargantext.Components.SessionLoader (sessionWrapper)
import Gargantext.Components.SimpleLayout (simpleLayout)
import Gargantext.Config (defaultFrontends, defaultBackends)
import Gargantext.Ends (Backend)
import Gargantext.Routes (AppRoute)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session)
import Gargantext.Types (CorpusId, ListId, NodeID, NodeType(..), SessionId)
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Router"

type Props = ( boxes :: Boxes, tasks :: T.Box (Maybe GAT.Reductor) )

type SessionProps = ( session :: R.Context Session, sessionId :: SessionId | Props )

type SessionNodeProps = ( nodeId :: NodeID | SessionProps )
type Props' = ( route' :: AppRoute, backend :: Backend | Props )

router :: R2.Leaf Props
router props = R.createElement routerCpt props []

routerCpt :: R.Component Props
routerCpt = here.component "router" cpt where
  cpt props@{ boxes, tasks } _ = do
    let session = R.createContext (unsafeCoerce {})
    let sessionProps sId = Record.merge { session, sessionId: sId } props
    let sessionNodeProps sId nId = Record.merge { nodeId: nId } $ sessionProps sId
    showLogin <- T.useLive T.unequal boxes.showLogin
    route' <- T.useLive T.unequal boxes.route
    pure $ R.fragment
      [ if showLogin then login' boxes else H.div {} []
      , case route' of
        GR.Annuaire s n           -> annuaire (sessionNodeProps s n) []
        GR.ContactPage s a n      -> contact (Record.merge { annuaireId: a } $ sessionNodeProps s n) []
        GR.Corpus s n             -> corpus (sessionNodeProps s n) []
        GR.CorpusDocument s c l n -> corpusDocument (Record.merge { corpusId: c, listId: l } $ sessionNodeProps s n) []
        GR.Dashboard s n          -> dashboard (sessionNodeProps s n) []
        GR.Document s l n         -> document (Record.merge { listId: l } $ sessionNodeProps s n) []
        GR.Folder        s n      -> corpus (sessionNodeProps s n) []
        GR.FolderPrivate s n      -> corpus (sessionNodeProps s n) []
        GR.FolderPublic  s n      -> corpus (sessionNodeProps s n) []
        GR.FolderShared  s n      -> corpus (sessionNodeProps s n) []
        GR.Home                   -> home props []
        GR.Lists s n              -> lists (sessionNodeProps s n) []
        GR.Login                  -> login' boxes
        GR.PGraphExplorer s g     -> graphExplorer (sessionNodeProps s g) []
        GR.RouteFile s n          -> routeFile (sessionNodeProps s n) []
        GR.RouteFrameCalc  s n    -> routeFrame (Record.merge { nodeType: NodeFrameCalc } $ sessionNodeProps s n) []
        GR.RouteFrameCode  s n    -> routeFrame (Record.merge { nodeType: NodeFrameNotebook } $ sessionNodeProps s n) []
        GR.RouteFrameWrite s n    -> routeFrame (Record.merge { nodeType: NodeFrameWrite } $ sessionNodeProps s n) []
        GR.Team s n               -> team (sessionNodeProps s n) []
        GR.Texts s n              -> texts (sessionNodeProps s n) []
        GR.UserPage s n           -> user (sessionNodeProps s n) []
      ]

forested :: R2.Component Props
forested = R.createElement forestedCpt

forestedCpt :: R.Component Props
forestedCpt = here.component "forested" cpt
  where
    cpt { boxes: { backend, handed, reloadForest, reloadRoot, route, sessions, showLogin }, tasks } children = do
      pure $ forestLayout { backend
                          , frontends: defaultFrontends
                          , handed
                          , reloadForest
                          , reloadRoot
                          , route
                          , sessions
                          , showLogin
                          , tasks } children

authed :: Record SessionProps -> R.Element -> R.Element
authed props@{ boxes: { sessions }, session, sessionId, tasks } content =
  sessionWrapper { fallback: home homeProps [], context: session, sessionId, sessions }
    [ content, footer { } [] ]
    where
      homeProps = RE.pick props :: Record Props

annuaire :: R2.Component SessionNodeProps
annuaire = R.createElement annuaireCpt

annuaireCpt :: R.Component SessionNodeProps
annuaireCpt = here.component "annuaire" cpt where
  cpt props@{ boxes, nodeId, session, sessionId, tasks } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    pure $ authed sessionProps $
      forested { boxes, tasks } [ annuaireLayout { frontends, nodeId, session } ]
      where frontends = defaultFrontends

corpus :: R2.Component SessionNodeProps
corpus = R.createElement corpusCpt

corpusCpt :: R.Component SessionNodeProps
corpusCpt = here.component "corpus" cpt where
  cpt props@{ boxes, nodeId, session, tasks } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    pure $ authed sessionProps $
      forested { boxes, tasks } [ corpusLayout { nodeId, session } ]

type CorpusDocumentProps =
  ( corpusId :: CorpusId
  , listId :: ListId
  | SessionNodeProps
  )

corpusDocument :: R2.Component CorpusDocumentProps
corpusDocument = R.createElement corpusDocumentCpt

corpusDocumentCpt :: R.Component CorpusDocumentProps
corpusDocumentCpt = here.component "corpusDocument" cpt
  where
    cpt props@{ boxes, corpusId: corpusId', listId, nodeId, session, sessionId, tasks } _ = do
      let sessionProps = RE.pick props :: Record SessionProps
      pure $ authed sessionProps $
        forested { boxes, tasks }
        [ documentMainLayout { mCorpusId: corpusId, listId: listId, nodeId, session } [] ]
        where corpusId = Just corpusId'

dashboard :: R2.Component SessionNodeProps
dashboard = R.createElement dashboardCpt

dashboardCpt :: R.Component SessionNodeProps
dashboardCpt = here.component "dashboard" cpt
  where
    cpt props@{ boxes, nodeId, session, tasks } _ = do
      let sessionProps = RE.pick props :: Record SessionProps
      pure $ authed sessionProps $
        forested { boxes, tasks } [ dashboardLayout { nodeId, session } [] ]

type DocumentProps = ( listId :: ListId | SessionNodeProps )

document :: R2.Component DocumentProps
document = R.createElement documentCpt

documentCpt :: R.Component DocumentProps
documentCpt = here.component "document" cpt where
  cpt props@{ listId, nodeId, session, sessionId, tasks, boxes } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    pure $ authed sessionProps $
      forested { boxes, tasks }
      [ documentMainLayout { listId, nodeId, mCorpusId, session } [] ]
      where mCorpusId = Nothing

home :: R2.Component Props
home = R.createElement homeCpt

homeCpt :: R.Component Props
homeCpt = here.component "home" cpt where
  cpt props@{ boxes: boxes@{ sessions, showLogin }, tasks } _ = do
    pure $ forested { boxes, tasks } [ homeLayout  { lang: LL_EN, sessions, showLogin } ]

lists :: R2.Component SessionNodeProps
lists = R.createElement listsCpt

listsCpt :: R.Component SessionNodeProps
listsCpt = here.component "lists" cpt where
  cpt props@{ boxes: { backend
                       , handed
                       , reloadForest
                       , reloadRoot
                       , route
                       , sessions
                       , showLogin }
            , nodeId
            , session
            , sessionId
            , tasks } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    session' <- R.useContext session
    pure $ authed sessionProps $
      Lists.listsWithForest
      { forestProps: { backend
                     , frontends
                     , handed
                     , reloadForest
                     , reloadRoot
                     , route
                     , sessions
                     , showLogin
                     , tasks }
      , listsProps: { nodeId
                    , reloadRoot
                    , reloadForest
                    , session: session'
                    , sessionUpdate: \_ -> pure unit
                    , tasks }
      } []
      where frontends = defaultFrontends

login' :: Boxes -> R.Element
login' { backend, sessions, showLogin: visible } =
  login { backend
        , backends: fromFoldable defaultBackends
        , sessions
        , visible }

graphExplorer :: R2.Component SessionNodeProps
graphExplorer = R.createElement graphExplorerCpt

graphExplorerCpt :: R.Component SessionNodeProps
graphExplorerCpt = here.component "graphExplorer" cpt where
  cpt props@{ boxes: { backend, handed, route, sessions, showLogin }
            , nodeId
            , session
            , tasks } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    pure $ authed sessionProps $
      simpleLayout { handed }
      [ explorerLayoutLoader { backend
                             , frontends
                             , graphId: nodeId
                             , handed
                             , route
                             , session
                             , sessions
                             , showLogin
                             , tasks } [] ]
      where frontends = defaultFrontends

routeFile :: R2.Component SessionNodeProps
routeFile = R.createElement routeFileCpt

routeFileCpt :: R.Component SessionNodeProps
routeFileCpt = here.component "routeFile" cpt where
  cpt props@{ nodeId, session, sessionId, boxes, tasks } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    pure $ authed sessionProps $
      forested { boxes, tasks } [ fileLayout { nodeId, session } ]

type RouteFrameProps = (
  nodeType :: NodeType
  | SessionNodeProps
  )

routeFrame :: R2.Component RouteFrameProps
routeFrame = R.createElement routeFrameCpt

routeFrameCpt :: R.Component RouteFrameProps
routeFrameCpt = here.component "routeFrame" cpt where
  cpt props@{ nodeId, nodeType, session, sessionId, boxes, tasks } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    pure $ authed sessionProps $ forested { boxes, tasks } [ frameLayout { nodeId, nodeType, session } ]

team :: R2.Component SessionNodeProps
team = R.createElement teamCpt

teamCpt :: R.Component SessionNodeProps
teamCpt = here.component "team" cpt where
  cpt props@{ nodeId, session, sessionId, boxes, tasks } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    pure $ authed sessionProps $ forested { boxes, tasks } [ corpusLayout { nodeId, session } ]

texts :: R2.Component SessionNodeProps
texts = R.createElement textsCpt

textsCpt :: R.Component SessionNodeProps
textsCpt = here.component "texts" cpt
  where
    cpt props@{ boxes: { backend
                         , handed
                         , reloadForest
                         , reloadRoot
                         , route
                         , sessions
                         , showLogin }
              , nodeId
              , session
              , sessionId
              , tasks } _ = do
      let sessionProps = RE.pick props :: Record SessionProps
      session' <- R.useContext session
      pure $ authed sessionProps $
        Texts.textsWithForest
        { forestProps: { backend
                       , frontends
                       , handed
                       , route
                       , reloadForest
                       , reloadRoot
                       , sessions
                       , showLogin
                       , tasks }
        , textsProps: { frontends
                      , nodeId
                      , session: session' } }
        [] where frontends = defaultFrontends

user :: R2.Component SessionNodeProps
user = R.createElement userCpt

userCpt :: R.Component SessionNodeProps
userCpt = here.component "user" cpt where
  cpt props@{ boxes: boxes@{ reloadForest, reloadRoot }
            , nodeId
            , session
            , sessionId
            , tasks } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    session' <- R.useContext session
    pure $ authed sessionProps $
      forested { boxes, tasks }
        [ userLayout { frontends
                     , nodeId
                     , reloadForest
                     , reloadRoot
                     , session: session'
                     , tasks } ]
      where frontends = defaultFrontends

type ContactProps = ( annuaireId :: NodeID | SessionNodeProps )

contact :: R2.Component ContactProps
contact = R.createElement contactCpt

contactCpt :: R.Component ContactProps
contactCpt = here.component "contact" cpt where
  cpt props@{ annuaireId, nodeId, session, sessionId, tasks
            , boxes: { reloadForest, reloadRoot } } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    let forestedProps = RE.pick props :: Record Props
    pure $ authed sessionProps $
      forested forestedProps
      [ contactLayout { annuaireId, frontends, nodeId, reloadForest, reloadRoot, session, tasks } [] ]
      where frontends = defaultFrontends
