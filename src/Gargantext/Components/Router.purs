module Gargantext.Components.Router (router) where

import Gargantext.Prelude

import Data.Array (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Record.Extra as RE
import Toestand as T
import Unsafe.Coerce (unsafeCoerce)

import Gargantext.Components.App.Data (Boxes)
import Gargantext.Components.Footer (footer)
import Gargantext.Components.Forest as Forest
import Gargantext.Components.GraphExplorer as GraphExplorer
import Gargantext.Components.GraphExplorer.Sidebar as GES
import Gargantext.Components.GraphExplorer.Sidebar.Types as GEST
import Gargantext.Components.Lang (LandingLang(LL_EN))
import Gargantext.Components.Login (login)
import Gargantext.Components.Nodes.Annuaire (annuaireLayout)
import Gargantext.Components.Nodes.Annuaire.User (userLayoutSessionContext)
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
import Gargantext.Components.TopBar as TopBar
import Gargantext.Config (defaultFrontends, defaultBackends)
import Gargantext.Ends (Backend)
import Gargantext.Routes (AppRoute)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, WithSessionContext)
import Gargantext.Types (CorpusId, ListId, NodeID, NodeType(..), SessionId, SidePanelState(..))
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Router"

type Props = ( boxes :: Boxes )

type SessionProps = ( session :: R.Context Session, sessionId :: SessionId | Props )

type SessionNodeProps = ( nodeId :: NodeID | SessionProps )
type Props' = ( route' :: AppRoute, backend :: Backend | Props )

router :: R2.Leaf Props
router props = R.createElement routerCpt props []

routerCpt :: R.Component Props
routerCpt = here.component "router" cpt where
  cpt props@{ boxes } _ = do
    let session = R.createContext (unsafeCoerce {})

    pure $ R.fragment
      [ loginModal { boxes } []
      , topBar { boxes } []
      , forest { boxes, session } []
      , sidePanel { boxes, session } []
      ]

renderRoute :: R2.Component (WithSessionContext Props)
renderRoute = R.createElement renderRouteCpt

renderRouteCpt :: R.Component (WithSessionContext Props)
renderRouteCpt = here.component "renderRoute" cpt where
  cpt props@{ boxes, session } _ = do
    let sessionProps sId = Record.merge { session, sessionId: sId } props
    let sessionNodeProps sId nId = Record.merge { nodeId: nId } $ sessionProps sId

    route' <- T.useLive T.unequal boxes.route

    pure $ R.fragment
      [ case route' of
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
        GR.Home                   -> home { boxes } []
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


loginModal :: R2.Component Props
loginModal = R.createElement loginModalCpt

loginModalCpt :: R.Component Props
loginModalCpt = here.component "loginModal" cpt
  where
    cpt { boxes: boxes@{ showLogin } } _ = do
        showLogin' <- T.useLive T.unequal showLogin

        pure $ if showLogin' then login' boxes else H.div {} []

authed :: Record SessionProps -> R.Element -> R.Element
authed props@{ boxes: { sessions }, session, sessionId } content =
  sessionWrapper { fallback: home homeProps []
                 , context: session
                 , sessionId
                 , sessions } [ content, footer {} [] ]
    where
      homeProps = RE.pick props :: Record Props

topBar :: R2.Component Props
topBar = R.createElement topBarCpt

topBarCpt :: R.Component Props
topBarCpt = here.component "topBar" cpt where
  cpt props@{ boxes: boxes@{ handed
                           , route } } _ = do
    route' <- T.useLive T.unequal boxes.route

    let children = case route' of
          GR.PGraphExplorer s g -> [ GraphExplorer.topBar { boxes } [] ]
          _                     -> []

    pure $ TopBar.topBar { handed } children


forest :: R2.Component (WithSessionContext Props)
forest = R.createElement forestCpt

forestCpt :: R.Component (WithSessionContext Props)
forestCpt = here.component "forest" cpt where
  cpt props@{ boxes: { backend
                     , forestOpen
                     , handed
                     , reloadForest
                     , reloadRoot
                     , route
                     , sessions
                     , showLogin
                     , showTree
                     , tasks }
            , session } _ = do
    session' <- R.useContext session

    pure $ Forest.forestLayoutMain { backend
                                   , forestOpen
                                   , frontends: defaultFrontends
                                   , handed
                                   , reloadForest
                                   , reloadRoot
                                   , route
                                   , sessions
                                   , showLogin
                                   , showTree
                                   , tasks } [ renderRoute (Record.merge { session } props) [] ]

sidePanel :: R2.Component (WithSessionContext Props)
sidePanel = R.createElement sidePanelCpt

sidePanelCpt :: R.Component (WithSessionContext Props)
sidePanelCpt = here.component "sidePanel" cpt where
  cpt props@{ boxes: boxes@{ graphVersion
                           , reloadForest
                           , sidePanelGraph
                           , sidePanelState
                           , sidePanelLists
                           , sidePanelTexts }
            , session } _ = do
    route' <- T.useLive T.unequal boxes.route
    session' <- R.useContext session
    sidePanelState' <- T.useLive T.unequal sidePanelState

    let className = "side-panel"

    case sidePanelState' of
      Opened ->
        case route' of
          GR.Lists s n -> do
            pure $ H.div { className }
              [ Lists.sidePanel { session: session'
                                , sidePanel: sidePanelLists
                                , sidePanelState } [] ]
          GR.PGraphExplorer s g -> do
            { mGraph, mMetaData, removedNodeIds, selectedNodeIds, sideTab } <- GEST.focusedSidePanel sidePanelGraph
            mGraph' <- T.useLive T.unequal mGraph
            mGraphMetaData' <- T.useLive T.unequal mMetaData

            case (mGraph' /\ mGraphMetaData') of
              (Nothing /\ _) -> pure $ H.div {} []
              (_ /\ Nothing) -> pure $ H.div {} []
              (Just graph /\ Just metaData) -> do
                pure $ H.div { className }
                  [ GES.sidebar { frontends: defaultFrontends
                                , graph
                                , graphId: g
                                , graphVersion
                                , metaData
                                , reloadForest
                                , removedNodeIds
                                , selectedNodeIds
                                , session: session'
                                , sideTab
                                } [] ]
          GR.Texts s n -> do
            pure $ H.div { className }
              [ Texts.sidePanel { session: session'
                                , sidePanel: sidePanelTexts
                                , sidePanelState } [] ]
          _ -> pure $ H.div {} []
      _ -> pure $ H.div {} []

annuaire :: R2.Component SessionNodeProps
annuaire = R.createElement annuaireCpt

annuaireCpt :: R.Component SessionNodeProps
annuaireCpt = here.component "annuaire" cpt where
  cpt props@{ boxes, nodeId, session, sessionId } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    pure $ authed sessionProps $ annuaireLayout { frontends: defaultFrontends
                                                , nodeId
                                                , session }

corpus :: R2.Component SessionNodeProps
corpus = R.createElement corpusCpt

corpusCpt :: R.Component SessionNodeProps
corpusCpt = here.component "corpus" cpt where
  cpt props@{ boxes, nodeId, session } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    pure $ authed sessionProps $ corpusLayout { nodeId, session }

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
    cpt props@{ boxes, corpusId: corpusId', listId, nodeId, session, sessionId } _ = do
      let sessionProps = RE.pick props :: Record SessionProps
      pure $ authed sessionProps $
        documentMainLayout { mCorpusId: corpusId, listId: listId, nodeId, session } []
        where corpusId = Just corpusId'

dashboard :: R2.Component SessionNodeProps
dashboard = R.createElement dashboardCpt

dashboardCpt :: R.Component SessionNodeProps
dashboardCpt = here.component "dashboard" cpt
  where
    cpt props@{ boxes, nodeId, session } _ = do
      let sessionProps = RE.pick props :: Record SessionProps
      pure $ authed sessionProps $ dashboardLayout { nodeId, session } []

type DocumentProps = ( listId :: ListId | SessionNodeProps )

document :: R2.Component DocumentProps
document = R.createElement documentCpt

documentCpt :: R.Component DocumentProps
documentCpt = here.component "document" cpt where
  cpt props@{ boxes, listId, nodeId, session, sessionId } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    pure $ authed sessionProps $
      documentMainLayout { listId, nodeId, mCorpusId, session } []
      where mCorpusId = Nothing

home :: R2.Component Props
home = R.createElement homeCpt

homeCpt :: R.Component Props
homeCpt = here.component "home" cpt where
  cpt props@{ boxes: boxes@{ sessions, showLogin } } _ = do
    pure $ homeLayout { lang: LL_EN, sessions, showLogin }

lists :: R2.Component SessionNodeProps
lists = R.createElement listsCpt

listsCpt :: R.Component SessionNodeProps
listsCpt = here.component "lists" cpt where
  cpt props@{ boxes: { backend
                     , forestOpen
                     , handed
                     , reloadForest
                     , reloadRoot
                     , route
                     , sessions
                     , showLogin
                     , sidePanelState
                     , sidePanelLists
                     , tasks }
            , nodeId
            , session
            , sessionId } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    pure $ authed sessionProps $
      Lists.listsWithSessionContext { nodeId
                                    , reloadForest
                                    , reloadRoot
                                    , session
                                    , sessionUpdate: \_ -> pure unit
                                    , sidePanel: sidePanelLists
                                    , sidePanelState
                                    , tasks } []

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
  cpt props@{ boxes: boxes@{ backend
                           , handed
                           , route
                           , sessions
                           , showLogin
                           , sidePanelGraph
                           , sidePanelState
                           , tasks }
            , nodeId
            , session } _ = do
    { mMetaData } <- GEST.focusedSidePanel sidePanelGraph
    mMetaData' <- T.useLive T.unequal mMetaData
    let sessionProps = RE.pick props :: Record SessionProps
    pure $ authed sessionProps $
      -- simpleLayout { handed }
      GraphExplorer.explorerLayoutLoader { backend
                                         , boxes
                                         , frontends: defaultFrontends
                                         , graphId: nodeId
                                         , handed
                                         , mMetaData'
                                         , route
                                         , session
                                         , sessions
                                         , showLogin
                                         , sidePanelState
                                         , tasks } []

routeFile :: R2.Component SessionNodeProps
routeFile = R.createElement routeFileCpt

routeFileCpt :: R.Component SessionNodeProps
routeFileCpt = here.component "routeFile" cpt where
  cpt props@{ boxes, nodeId, session, sessionId } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    pure $ authed sessionProps $
      fileLayout { nodeId, session }

type RouteFrameProps = (
  nodeType :: NodeType
  | SessionNodeProps
  )

routeFrame :: R2.Component RouteFrameProps
routeFrame = R.createElement routeFrameCpt

routeFrameCpt :: R.Component RouteFrameProps
routeFrameCpt = here.component "routeFrame" cpt where
  cpt props@{ boxes, nodeId, nodeType, session, sessionId } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    pure $ authed sessionProps $
      frameLayout { nodeId, nodeType, session }

team :: R2.Component SessionNodeProps
team = R.createElement teamCpt

teamCpt :: R.Component SessionNodeProps
teamCpt = here.component "team" cpt where
  cpt props@{ boxes, nodeId, session, sessionId } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    pure $ authed sessionProps $
      corpusLayout { nodeId, session }

texts :: R2.Component SessionNodeProps
texts = R.createElement textsCpt

textsCpt :: R.Component SessionNodeProps
textsCpt = here.component "texts" cpt
  where
    cpt props@{ boxes: { backend
                       , forestOpen
                       , handed
                       , reloadForest
                       , reloadRoot
                       , route
                       , sessions
                       , showLogin
                       , sidePanelState
                       , sidePanelTexts
                       , tasks }
              , nodeId
              , session
              , sessionId } _ = do
      let sessionProps = RE.pick props :: Record SessionProps
      pure $ authed sessionProps $
        Texts.textsWithSessionContext { frontends: defaultFrontends
                                      , nodeId
                                      , session
                                      , sidePanel: sidePanelTexts
                                      , sidePanelState } []

user :: R2.Component SessionNodeProps
user = R.createElement userCpt

userCpt :: R.Component SessionNodeProps
userCpt = here.component "user" cpt where
  cpt props@{ boxes: boxes@{ reloadForest
                           , reloadRoot
                           , sidePanelState
                           , sidePanelTexts
                           , tasks }
            , nodeId
            , session
            , sessionId } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    pure $ authed sessionProps $
      userLayoutSessionContext { frontends: defaultFrontends
                               , nodeId
                               , reloadForest
                               , reloadRoot
                               , session
                               , sidePanel: sidePanelTexts
                               , sidePanelState
                               , tasks } []

type ContactProps = ( annuaireId :: NodeID | SessionNodeProps )

contact :: R2.Component ContactProps
contact = R.createElement contactCpt

contactCpt :: R.Component ContactProps
contactCpt = here.component "contact" cpt where
  cpt props@{ annuaireId
            , boxes: { reloadForest
                     , reloadRoot
                     , sidePanelTexts
                     , sidePanelState
                     , tasks }
            , nodeId
            , session
            , sessionId } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    let forestedProps = RE.pick props :: Record Props
    pure $ authed sessionProps $
      contactLayout { annuaireId
                    , frontends: defaultFrontends
                    , nodeId
                    , reloadForest
                    , reloadRoot
                    , session
                    , sidePanel: sidePanelTexts
                    , sidePanelState
                    , tasks } []
