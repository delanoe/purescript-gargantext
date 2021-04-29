module Gargantext.Components.Router (router) where

import Gargantext.Prelude

import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Gargantext.Components.App.Data (Boxes)
import Gargantext.Components.Footer (footer)
import Gargantext.Components.Forest as Forest
import Gargantext.Components.GraphExplorer as GraphExplorer
import Gargantext.Components.GraphExplorer.Sidebar as GES
import Gargantext.Components.GraphExplorer.Sidebar.Types as GEST
import Gargantext.Components.Lang (LandingLang(LL_EN))
import Gargantext.Components.Login (login)
import Gargantext.Components.MainPage as MainPage
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
import Gargantext.Components.TopBar as TopBar
import Gargantext.Config (defaultFrontends, defaultBackends)
import Gargantext.Ends (Backend)
import Gargantext.Routes (AppRoute)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, WithSession)
import Gargantext.Sessions as Sessions
import Gargantext.Types (CorpusId, Handed(..), ListId, NodeID, NodeType(..), SessionId, SidePanelState(..), reverseHanded)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Record.Extra as RE
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Router"

type Props = ( boxes :: Boxes )

type SessionProps = ( sessionId :: SessionId | Props )

type SessionNodeProps = ( nodeId :: NodeID | SessionProps )
type Props' = ( backend :: Backend, route' :: AppRoute | Props )

router :: R2.Leaf Props
router props = R.createElement routerCpt props []
routerCpt :: R.Component Props
routerCpt = here.component "router" cpt where
  cpt props@{ boxes: boxes@{ handed } } _ = do
    handed'       <- T.useLive T.unequal handed

    let handedClassName = case handed' of
          LeftHanded  -> "left-handed"
          RightHanded -> "right-handed"

    pure $ R.fragment
      ([ loginModal { boxes } []
       , topBar { boxes } [] ] <>
       [ H.div { className: handedClassName } $ reverseHanded handed' $
         [ forest { boxes } []
         , mainPage { boxes } []
         , sidePanel { boxes } []
         ]
       ])


loginModal :: R2.Component Props
loginModal = R.createElement loginModalCpt
loginModalCpt :: R.Component Props
loginModalCpt = here.component "loginModal" cpt
  where
    cpt { boxes: boxes@{ showLogin } } _ = do
        showLogin' <- T.useLive T.unequal showLogin

        pure $ if showLogin' then login' boxes else H.div {} []

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

mainPage :: R2.Component Props
mainPage = R.createElement mainPageCpt
mainPageCpt :: R.Component Props
mainPageCpt = here.component "mainPage" cpt where
  cpt { boxes } _ = do
    pure $ MainPage.mainPage {} [ renderRoute { boxes }































































[] ]

forest :: R2.Component Props
forest = R.createElement forestCpt
forestCpt :: R.Component Props
forestCpt = here.component "forest" cpt where
  cpt props@{ boxes: boxes@{ backend
                           , forestOpen
                           , handed
                           , reloadForest
                           , reloadMainPage
                           , reloadRoot
                           , route
                           , sessions
                           , showLogin
                           , showTree
                           , tasks } } _ = do
    pure $ Forest.forestLayout { backend
                               , forestOpen
                               , frontends: defaultFrontends
                               , handed
                               , reloadForest
                               , reloadMainPage
                               , reloadRoot
                               , route
                               , sessions
                               , showLogin
                               , showTree
                               , tasks } []

sidePanel :: R2.Component Props
sidePanel = R.createElement sidePanelCpt
sidePanelCpt :: R.Component Props
sidePanelCpt = here.component "sidePanel" cpt where
  cpt props@{ boxes: boxes@{ graphVersion
                           , reloadForest
                           , session
                           , sidePanelGraph
                           , sidePanelState
                           , sidePanelLists
                           , sidePanelTexts } } _ = do
    session' <- T.useLive T.unequal session
    sidePanelState' <- T.useLive T.unequal sidePanelState

    case session' of
      Nothing -> pure $ H.div {} []
      Just s  ->
        case sidePanelState' of
          Opened -> pure $ openedSidePanel (Record.merge { session: s } props) []
          _      -> pure $ H.div {} []

renderRoute :: R2.Component Props
renderRoute = R.createElement renderRouteCpt
renderRouteCpt :: R.Component Props
renderRouteCpt = here.component "renderRoute" cpt where
  cpt props@{ boxes } _ = do
    let sessionNodeProps sId nId = Record.merge { nodeId: nId, sessionId: sId } props

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

type AuthedProps =
  ( content :: Session -> R.Element
  | SessionProps )

authed :: R2.Component AuthedProps
authed = R.createElement authedCpt
authedCpt :: R.Component AuthedProps
authedCpt = here.component "authed" cpt where
  cpt props@{ boxes: { session, sessions }
            , content
            , sessionId } _ = do
    sessions' <- T.useLive T.unequal sessions
    let session' = Sessions.lookup sessionId sessions'

    R.useEffect' $ do
      T.write_ session' session

    case session' of
      Nothing -> pure $ home homeProps []
      Just s  -> pure $ R.fragment [ content s, footer {} [] ]
    where
      homeProps = RE.pick props :: Record Props

openedSidePanel :: R2.Component (WithSession Props)
openedSidePanel = R.createElement openedSidePanelCpt

openedSidePanelCpt :: R.Component (WithSession Props)
openedSidePanelCpt = here.component "openedSidePanel" cpt where
  cpt props@{ boxes: boxes@{ graphVersion
                           , reloadForest
                           , route
                           , sidePanelGraph
                           , sidePanelState
                           , sidePanelLists
                           , sidePanelTexts }
            , session} _ = do
    { mGraph, mMetaData, removedNodeIds, selectedNodeIds, sideTab } <- GEST.focusedSidePanel sidePanelGraph
    mGraph' <- T.useLive T.unequal mGraph
    mGraphMetaData' <- T.useLive T.unequal mMetaData
    route' <- T.useLive T.unequal route

    let wrapper = H.div { className: "side-panel" }

    case route' of
      GR.Lists s n -> do
        pure $ wrapper
          [ Lists.sidePanel { session
                            , sidePanel: sidePanelLists
                            , sidePanelState } [] ]
      GR.PGraphExplorer s g -> do
        case (mGraph' /\ mGraphMetaData') of
          (Nothing /\ _) -> pure $ wrapper []
          (_ /\ Nothing) -> pure $ wrapper []
          (Just graph /\ Just metaData) -> do
            pure $ wrapper
              [ GES.sidebar { frontends: defaultFrontends
                            , graph
                            , graphId: g
                            , graphVersion
                            , metaData
                            , reloadForest
                            , removedNodeIds
                            , selectedNodeIds
                            , session
                            , sideTab
                            } [] ]
      GR.Texts s n -> do
        pure $ wrapper
          [ Texts.sidePanel { session
                            , sidePanel: sidePanelTexts
                            , sidePanelState } [] ]
      _ -> pure $ wrapper []

annuaire :: R2.Component SessionNodeProps
annuaire = R.createElement annuaireCpt

annuaireCpt :: R.Component SessionNodeProps
annuaireCpt = here.component "annuaire" cpt where
  cpt props@{ boxes, nodeId } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    pure $ authed (Record.merge { content: \session ->
                                   annuaireLayout { frontends: defaultFrontends
                                                  , nodeId
                                                  , session } } sessionProps) []

corpus :: R2.Component SessionNodeProps
corpus = R.createElement corpusCpt

corpusCpt :: R.Component SessionNodeProps
corpusCpt = here.component "corpus" cpt where
  cpt props@{ boxes, nodeId } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    pure $ authed (Record.merge { content: \session ->
                                   corpusLayout { nodeId, session } } sessionProps) []

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
    cpt props@{ boxes, corpusId: corpusId', listId, nodeId } _ = do
      let sessionProps = RE.pick props :: Record SessionProps
      pure $ authed (Record.merge { content: \session ->
                                     documentMainLayout { mCorpusId: Just corpusId'
                                                        , listId: listId
                                                        , nodeId
                                                        , session } [] } sessionProps )[]

dashboard :: R2.Component SessionNodeProps
dashboard = R.createElement dashboardCpt

dashboardCpt :: R.Component SessionNodeProps
dashboardCpt = here.component "dashboard" cpt
  where
    cpt props@{ boxes, nodeId } _ = do
      let sessionProps = RE.pick props :: Record SessionProps
      pure $ authed (Record.merge { content: \session ->
                                     dashboardLayout { nodeId, session } [] } sessionProps) []

type DocumentProps = ( listId :: ListId | SessionNodeProps )

document :: R2.Component DocumentProps
document = R.createElement documentCpt

documentCpt :: R.Component DocumentProps
documentCpt = here.component "document" cpt where
  cpt props@{ boxes, listId, nodeId } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    pure $ authed (Record.merge { content: \session ->
                                   documentMainLayout { listId
                                                      , nodeId
                                                      , mCorpusId: Nothing
                                                      , session } [] } sessionProps) []

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
            , nodeId } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    pure $ authed (Record.merge { content: \session ->
                                   -- simpleLayout { handed }
                                   GraphExplorer.explorerLayout { backend
                                                                , boxes
                                                                , frontends: defaultFrontends
                                                                , graphId: nodeId
                                                                , handed
                                                                , route
                                                                , session
                                                                , sessions
                                                                , showLogin
                                                                , tasks } [] } sessionProps) []

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
                     , reloadMainPage
                     , reloadRoot
                     , route
                     , sessions
                     , showLogin
                     , sidePanelState
                     , sidePanelLists
                     , tasks }
            , nodeId } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    pure $ authed (Record.merge { content: \session ->
                                   Lists.listsLayout { nodeId
                                                     , reloadForest
                                                     , reloadMainPage
                                                     , reloadRoot
                                                     , session
                                                     , sessionUpdate: \_ -> pure unit
                                                     , sidePanel: sidePanelLists
                                                     , sidePanelState
                                                     , tasks } [] } sessionProps) []

login' :: Boxes -> R.Element
login' { backend, sessions, showLogin: visible } =
  login { backend
        , backends: A.fromFoldable defaultBackends
        , sessions
        , visible }

routeFile :: R2.Component SessionNodeProps
routeFile = R.createElement routeFileCpt

routeFileCpt :: R.Component SessionNodeProps
routeFileCpt = here.component "routeFile" cpt where
  cpt props@{ boxes, nodeId } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    pure $ authed (Record.merge { content: \session ->
                                   fileLayout { nodeId, session } } sessionProps) []

type RouteFrameProps = (
  nodeType :: NodeType
  | SessionNodeProps
  )

routeFrame :: R2.Component RouteFrameProps
routeFrame = R.createElement routeFrameCpt

routeFrameCpt :: R.Component RouteFrameProps
routeFrameCpt = here.component "routeFrame" cpt where
  cpt props@{ boxes, nodeId, nodeType } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    pure $ authed (Record.merge { content: \session ->
                                   frameLayout { nodeId, nodeType, session } } sessionProps) []

team :: R2.Component SessionNodeProps
team = R.createElement teamCpt

teamCpt :: R.Component SessionNodeProps
teamCpt = here.component "team" cpt where
  cpt props@{ boxes, nodeId } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    pure $ authed (Record.merge { content: \session ->
                                   corpusLayout { nodeId, session } } sessionProps) []

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
              , nodeId } _ = do
      let sessionProps = RE.pick props :: Record SessionProps
      pure $ authed (Record.merge { content: \session ->
                                     Texts.textsLayout { frontends: defaultFrontends
                                                       , nodeId
                                                       , session
                                                       , sidePanel: sidePanelTexts
                                                       , sidePanelState } [] } sessionProps) []

user :: R2.Component SessionNodeProps
user = R.createElement userCpt

userCpt :: R.Component SessionNodeProps
userCpt = here.component "user" cpt where
  cpt props@{ boxes: boxes@{ reloadForest
                           , reloadRoot
                           , sidePanelState
                           , sidePanelTexts
                           , tasks }
            , nodeId } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    pure $ authed (Record.merge { content: \session ->
                                   userLayout { frontends: defaultFrontends
                                              , nodeId
                                              , reloadForest
                                              , reloadRoot
                                              , session
                                              , sidePanel: sidePanelTexts
                                              , sidePanelState
                                              , tasks } [] } sessionProps) []

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
            , nodeId } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    let forestedProps = RE.pick props :: Record Props
    pure $ authed (Record.merge { content: \session ->
                                   contactLayout { annuaireId
                                                 , frontends: defaultFrontends
                                                 , nodeId
                                                 , reloadForest
                                                 , reloadRoot
                                                 , session
                                                 , sidePanel: sidePanelTexts
                                                 , sidePanelState
                                                 , tasks } [] } sessionProps) []
