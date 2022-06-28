module Gargantext.Components.Router (router) where

import Gargantext.Prelude

import Data.Array (filter, length)
import Data.Array as A
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.UUID (UUID)
import Data.UUID as UUID
import Effect (Effect)
import Gargantext.Components.App.Store (Boxes)
import Gargantext.Components.ErrorsView (errorsView)
import Gargantext.Components.Forest (forestLayout)
import Gargantext.Components.Login (login)
import Gargantext.Components.Nodes.Annuaire (annuaireLayout)
import Gargantext.Components.Nodes.Annuaire.User (userLayout)
import Gargantext.Components.Nodes.Annuaire.User.Contact (contactLayout)
import Gargantext.Components.Nodes.Corpus (corpusLayout)
import Gargantext.Components.Nodes.Corpus.Code (corpusCodeLayout)
import Gargantext.Components.Nodes.Corpus.Dashboard (dashboardLayout)
import Gargantext.Components.Nodes.Corpus.Document as Document
import Gargantext.Components.Nodes.Corpus.Graph as Graph
import Gargantext.Components.Nodes.Corpus.Phylo as Phylo
import Gargantext.Components.Nodes.File (fileLayout)
import Gargantext.Components.Nodes.Frame as Frame
import Gargantext.Components.Nodes.Home (homeLayout)
import Gargantext.Components.Nodes.Lists as Lists
import Gargantext.Components.Nodes.Team (teamLayout)
import Gargantext.Components.Nodes.Texts as Texts
import Gargantext.Components.Tile (tileBlock)
import Gargantext.Components.TopBar as TopBar
import Gargantext.Config (defaultFrontends, defaultBackends)
import Gargantext.Context.Session as SessionContext
import Gargantext.Ends (Backend)
import Gargantext.Hooks.Resize (ResizeType(..), useResizeHandler)
import Gargantext.Hooks.Session (useSession)
import Gargantext.Routes (AppRoute, Tile)
import Gargantext.Routes as GR
import Gargantext.Sessions (Session, sessionId)
import Gargantext.Sessions as Sessions
import Gargantext.Types (CorpusId, Handed(..), ListId, NodeID, NodeType(..), SessionId, SidePanelState(..))
import Gargantext.Utils ((?))
import Gargantext.Utils.Reactix (getElementById)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Record (get)
import Record as Record
import Record.Extra as RE
import Toestand as T
import Type.Proxy (Proxy(..))

here :: R2.Here
here = R2.here "Gargantext.Components.Router"

type Props = ( boxes :: Boxes )

type SessionProps = ( sessionId :: SessionId | Props )

type SessionNodeProps = ( nodeId :: NodeID | SessionProps )
type Props' = ( backend :: Backend, route' :: AppRoute | Props )

router :: R2.Leaf Props
router = R2.leafComponent routerCpt
routerCpt :: R.Component Props
routerCpt = here.component "router" cpt where
  cpt { boxes: boxes@{ handed, showLogin } } _ = do
    -- States
    handed'     <- R2.useLive' handed
    showLogin'  <- R2.useLive' showLogin

    -- Effects
    let
      handedClassName = case _ of
        LeftHanded  -> "left-handed"
        RightHanded -> "right-handed"

    R.useEffect1' handed' $
      getElementById "app" >>= case _ of
        Nothing  -> pure unit
        Just app -> do
          R2.removeClass app
            [ handedClassName LeftHanded
            , handedClassName RightHanded
            ]
          R2.addClass app [ handedClassName handed' ]

    -- Render
    pure $

      H.div
      { className: "router" }
      [
        -- loginModal { boxes }
         R2.when showLogin' $
            login' boxes
       , TopBar.topBar { boxes }
       , errorsView { errors: boxes.errors } []
       , H.div { className: "router__inner" }
         [
          forest { boxes }
         ,
          mainPage { boxes }
         ,
          sidePanel { boxes }
         ]
       ]

--------------------------------------------------------------

mainPage :: R2.Leaf Props
mainPage = R2.leafComponent mainPageCpt
mainPageCpt :: R.Component Props
mainPageCpt = here.component "mainPage" cpt where
  cpt { boxes } _ = do
    -- States
    route         <- R2.useLive' boxes.route
    tileAxisXList <- R2.useLive' boxes.tileAxisXList
    tileAxisYList <- R2.useLive' boxes.tileAxisYList
    -- Computed
    let
      findTile :: UUID -> Record Tile -> Boolean
      findTile id tile = eq id $ get (Proxy :: Proxy "id") tile

      deleteTile ::
           Record Tile
        -> T.Box (Array (Record Tile))
        -> (Unit -> Effect Unit)
      deleteTile tile listBox = const do
        list <- T.read listBox
        newList <- pure $ filter (_ # tile.id # findTile # not) list
        T.write_ newList listBox

    let hasHorizontalTiles = not $ eq 0 $ length tileAxisXList
    let hasVerticalTiles = not $ eq 0 $ length tileAxisYList
    -- Render
    pure $

      H.div { className: "router__body main-page" }
      [
        H.div
        { className: intercalate " "
          [ "main-page__main-row"
          , if (hasVerticalTiles)
            then "main-page__main-row--with-y-tiles"
            else ""
          , if (hasVerticalTiles && not hasHorizontalTiles)
            then "main-page__main-row--only-y-tiles"
            else ""
          ]
        }
        [
          -- main render route
          H.div { className: "main-page__main-route" }
          [
            renderRoute { boxes, route }
          ]
        ,
          -- optional tile render route [Y Axis ~ aside vertical column]
          case tileAxisYList of
            [] -> mempty
            _  ->
              H.div
              { className: intercalate " "
                [ "main-page__vertical-tiles"
                , "main-page__vertical-tiles--" <> (show $ length tileAxisYList)
                ]
              } $
              tileAxisYList <#> \tile -> tileBlock
                { boxes
                , tile
                , key: UUID.toString tile.id
                , closeCallback: deleteTile tile boxes.tileAxisYList
                }
                [
                  renderRoute { boxes, route: tile.route }
                ]
        ]

      ,
        -- optional tile render route [X Axis ~ bottom horizontal row]
        case tileAxisXList of
          [] -> mempty
          _  ->
            H.div
            { className: intercalate " "
              [ "main-page__horizontal-tiles"
              , "main-page__horizontal-tiles--" <> (show $ length tileAxisXList)
              ]
            } $
            tileAxisXList <#> \tile -> tileBlock
              { boxes
              , tile
              , key: UUID.toString tile.id
              , closeCallback: deleteTile tile boxes.tileAxisXList
              }
              [
                renderRoute { boxes, route: tile.route }
              ]
      ]

--------------------------------------------------------------

forest :: R2.Leaf Props
forest = R2.leaf forestCpt

forestCpt :: R.Memo Props
forestCpt = R.memo' $ here.component "forest" cpt where
  cpt { boxes } _ = do
    -- States
    showTree' <- R2.useLive' boxes.showTree

    -- Hooks
    resizeHandler <- useResizeHandler

    -- Effects
    R.useLayoutEffect1' [] do
      resizeHandler.add
        ".router__aside__handle__action"
        ".router__aside"
        Vertical
      pure $ resizeHandler.remove
        ".router__aside__handle__action"

    -- Render
    pure $

      H.div
      { className: "router__aside"
      -- @XXX: ReactJS lack of "keep-alive" feature workaround solution
      -- @link https://github.com/facebook/react/issues/12039
      , style: { display: showTree' ? "block" $ "none" }
      }
      [
        H.div
        { className: "router__aside__inner" }
        [
          forestLayout
          { boxes
          , frontends: defaultFrontends
          }
        ]
      ,
        H.div
        { className: "router__aside__handle"
        }
        [
          H.div
          { className: "router__aside__handle__action" }
          []
        ]
      ]

--------------------------------------------------------------

sidePanel :: R2.Leaf Props
sidePanel = R2.leafComponent sidePanelCpt
sidePanelCpt :: R.Component Props
sidePanelCpt = here.component "sidePanel" cpt where
  cpt props@{ boxes: { session
                     , sidePanelState } } _ = do
    session' <- T.useLive T.unequal session
    sidePanelState' <- T.useLive T.unequal sidePanelState

    case session' of
      Nothing -> pure $ H.div {} []
      Just _  ->
        case sidePanelState' of
          Opened -> pure $
            R.provideContext SessionContext.context session'
            [ openedSidePanel props ]
          _      -> pure $ H.div {} []

--------------------------------------------------------------

type RenderRouteProps =
  ( route :: AppRoute
  | Props
  )

renderRoute :: R2.Leaf RenderRouteProps
renderRoute = R2.leaf renderRouteCpt

renderRouteCpt :: R.Memo RenderRouteProps
renderRouteCpt = R.memo' $ here.component "renderRoute" cpt where
  cpt { boxes, route } _ = do
    let sessionNodeProps sId nId =
          { nodeId: nId
          , sessionId: sId
          , boxes
          }

    pure $ R.fragment
      [ case route of
        GR.Annuaire s n           -> annuaire (sessionNodeProps s n) []
        GR.ContactPage s a n      -> contact (Record.merge { annuaireId: a } $ sessionNodeProps s n) []
        GR.Corpus s n             -> corpus (sessionNodeProps s n) []
        GR.CorpusCode s n         -> corpusCode (sessionNodeProps s n) []
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
        GR.PhyloExplorer s g      -> phyloExplorer (sessionNodeProps s g) []
        GR.RouteFile s n          -> routeFile (sessionNodeProps s n) []
        GR.RouteFrameWrite s n    -> routeFrame (Record.merge { nodeType: NodeFrameWrite    } $ sessionNodeProps s n) []
        GR.RouteFrameCalc  s n    -> routeFrame (Record.merge { nodeType: NodeFrameCalc     } $ sessionNodeProps s n) []
        GR.RouteFrameCode  s n    -> routeFrame (Record.merge { nodeType: NodeFrameNotebook } $ sessionNodeProps s n) []
        GR.RouteFrameVisio s n    -> routeFrame (Record.merge { nodeType: NodeFrameVisio    } $ sessionNodeProps s n) []
        GR.Team s n               -> team (sessionNodeProps s n) []
        GR.NodeTexts s n          -> texts (sessionNodeProps s n) []
        GR.UserPage s n           -> user (sessionNodeProps s n) []
      ]

--------------------------------------------------------------

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
      Nothing -> pure $
        home homeProps []
      Just s -> pure $
        R.provideContext SessionContext.context session' [ content s ]
    where
      homeProps = RE.pick props :: Record Props

--------------------------------------------------------------

openedSidePanel :: R2.Leaf Props
openedSidePanel = R2.leaf openedSidePanelCpt

openedSidePanelCpt :: R.Component Props
openedSidePanelCpt = here.component "openedSidePanel" cpt where
  cpt { boxes:
        { route
        , sidePanelState
        }
      } _ = do
    session <- useSession

    route' <- T.useLive T.unequal route

    let wrapper = H.div { className: "side-panel" }

    case route' of
      GR.Lists _s _n -> do
        pure $ wrapper
          [ Lists.sidePanel { session
                            , sidePanelState } [] ]
      GR.NodeTexts _s _n ->
        pure $ wrapper [ Texts.textsSidePanel {} ]
      _ -> pure $ wrapper []

--------------------------------------------------------------

annuaire :: R2.Component SessionNodeProps
annuaire = R.createElement annuaireCpt
annuaireCpt :: R.Component SessionNodeProps
annuaireCpt = here.component "annuaire" cpt where
  cpt props@{ nodeId } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    pure $ authed (Record.merge { content: \session ->
                                   annuaireLayout { frontends: defaultFrontends
                                                  , nodeId
                                                  , session } } sessionProps) []

--------------------------------------------------------------

corpus :: R2.Component SessionNodeProps
corpus = R.createElement corpusCpt
corpusCpt :: R.Component SessionNodeProps
corpusCpt = here.component "corpus" cpt where
  cpt props@{ boxes, nodeId } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    pure $ authed (Record.merge { content: \session ->
                                   corpusLayout { boxes
                                                , nodeId
                                                , session } } sessionProps) []

--------------------------------------------------------------

corpusCode :: R2.Component SessionNodeProps
corpusCode = R.createElement corpusCodeCpt
corpusCodeCpt :: R.Component SessionNodeProps
corpusCodeCpt = here.component "corpusCode" cpt where
  cpt props@{ boxes, nodeId } _ = do
    let
      sessionProps = RE.pick props :: Record SessionProps

      authedProps = Record.merge
        { content: \session -> corpusCodeLayout
            { nodeId
            , session
            , boxes
            }
        }
        sessionProps

    pure $ authed authedProps []

--------------------------------------------------------------

type CorpusDocumentProps =
  ( corpusId :: CorpusId
  , listId :: ListId
  | SessionNodeProps
  )

corpusDocument :: R2.Component CorpusDocumentProps
corpusDocument = R.createElement corpusDocumentCpt

corpusDocumentCpt :: R.Component CorpusDocumentProps
corpusDocumentCpt = here.component "corpusDocument" cpt where
  cpt props@{ corpusId, listId, nodeId } _ = do
    let
      sessionProps = (RE.pick props :: Record SessionProps)

      authedProps =
        Record.merge
        { content:
            \session ->
              Document.node
              { mCorpusId: Just corpusId
              , listId
              , nodeId
              , key: show (sessionId session) <> "-" <> show nodeId
              }
        }
        sessionProps

    pure $ authed authedProps []

--------------------------------------------------------------

dashboard :: R2.Component SessionNodeProps
dashboard = R.createElement dashboardCpt
dashboardCpt :: R.Component SessionNodeProps
dashboardCpt = here.component "dashboard" cpt
  where
    cpt props@{ boxes, nodeId } _ = do
      let sessionProps = RE.pick props :: Record SessionProps
      pure $ authed (Record.merge { content: \session ->
                                     dashboardLayout { boxes, nodeId, session } [] } sessionProps) []

--------------------------------------------------------------

type DocumentProps =
  ( listId :: ListId
  | SessionNodeProps
  )

document :: R2.Component DocumentProps
document = R.createElement documentCpt

documentCpt :: R.Component DocumentProps
documentCpt = here.component "document" cpt where
  cpt props@{ listId, nodeId } _ = do
    let
      sessionProps = (RE.pick props :: Record SessionProps)

      authedProps =
        Record.merge
        { content:
            \session ->
              Document.node
              { mCorpusId: Nothing
              , listId
              , nodeId
              , key: show (sessionId session) <> "-" <> show nodeId
              }
        }
        sessionProps

    pure $ authed authedProps []

--------------------------------------------------------------

graphExplorer :: R2.Component SessionNodeProps
graphExplorer = R.createElement graphExplorerCpt

graphExplorerCpt :: R.Component SessionNodeProps
graphExplorerCpt = here.component "graphExplorer" cpt where
  cpt props@{ nodeId } _ = do
    let
      sessionProps = (RE.pick props :: Record SessionProps)

      authedProps =
        Record.merge
        { content:
            \_ -> Graph.node
                  { graphId: nodeId
                  , key: "graphId-" <> show nodeId
                  }

        }
        sessionProps

    pure $ authed authedProps []

--------------------------------------------------------------

phyloExplorer :: R2.Component SessionNodeProps
phyloExplorer = R.createElement phyloExplorerCpt

phyloExplorerCpt :: R.Component SessionNodeProps
phyloExplorerCpt = here.component "phylo" cpt where
  cpt props@{ nodeId } _ = do
    let
      sessionProps = (RE.pick props :: Record SessionProps)

      authedProps =
        Record.merge
        { content:
            \_ -> Phylo.node
                  { nodeId
                  }
        }
        sessionProps

    pure $ authed authedProps []

--------------------------------------------------------------

home :: R2.Component Props
home = R.createElement homeCpt
homeCpt :: R.Component Props
homeCpt = here.component "home" cpt where
  cpt { boxes } _ = do
    pure $ homeLayout { boxes }

--------------------------------------------------------------

lists :: R2.Component SessionNodeProps
lists = R.createElement listsCpt
listsCpt :: R.Component SessionNodeProps
listsCpt = here.component "lists" cpt where
  cpt props@{ boxes
            , nodeId } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    pure $ authed (Record.merge { content: \session ->
                                   Lists.listsLayout { boxes
                                                     , nodeId
                                                     , session
                                                     , sessionUpdate: \_ -> pure unit } [] } sessionProps) []

--------------------------------------------------------------

login' :: Boxes -> R.Element
login' { backend, sessions, showLogin: visible } =
  login { backend
        , backends: A.fromFoldable defaultBackends
        , sessions
        , visible }

--------------------------------------------------------------

routeFile :: R2.Component SessionNodeProps
routeFile = R.createElement routeFileCpt
routeFileCpt :: R.Component SessionNodeProps
routeFileCpt = here.component "routeFile" cpt where
  cpt props@{ nodeId } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    pure $ authed (Record.merge { content: \session ->
                                   fileLayout { nodeId, session } } sessionProps) []

--------------------------------------------------------------

type RouteFrameProps =
  ( nodeType :: NodeType
  | SessionNodeProps
  )

routeFrame :: R2.Component RouteFrameProps
routeFrame = R.createElement routeFrameCpt

routeFrameCpt :: R.Component RouteFrameProps
routeFrameCpt = here.component "routeFrame" cpt where
  cpt props@{ nodeId, nodeType } _ = do
    let
      sessionProps = (RE.pick props :: Record SessionProps)

      authedProps =
        Record.merge
        { content:
            \session ->
              Frame.node
              { nodeId
              , nodeType
              , key: show (sessionId session) <> "-" <> show nodeId
              }

        }
        sessionProps

    pure $ authed authedProps []

--------------------------------------------------------------

team :: R2.Component SessionNodeProps
team = R.createElement teamCpt
teamCpt :: R.Component SessionNodeProps
teamCpt = here.component "team" cpt where
  cpt props@{ boxes, nodeId } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    pure $ authed (Record.merge { content: \session ->
                                   teamLayout { boxes
                                                , nodeId
                                                , session } } sessionProps) []

--------------------------------------------------------------

texts :: R2.Component SessionNodeProps
texts = R.createElement textsCpt

textsCpt :: R.Component SessionNodeProps
textsCpt = here.component "texts" cpt where
  cpt props@{ nodeId } _ = do
    let
      sessionProps = (RE.pick props :: Record SessionProps)

      authedProps =
        Record.merge
        { content:
            \_ -> Texts.textsLayout
                  { frontends: defaultFrontends
                  , nodeId
                  }
        }
        sessionProps

    pure $ authed authedProps []

--------------------------------------------------------------

user :: R2.Component SessionNodeProps
user = R.createElement userCpt
userCpt :: R.Component SessionNodeProps
userCpt = here.component "user" cpt where
  cpt props@{ boxes
            , nodeId } _ = do
    let sessionProps = RE.pick props :: Record SessionProps
    pure $ authed (Record.merge { content: \session ->
                                   userLayout { boxes
                                              , frontends: defaultFrontends
                                              , nodeId
                                              , session } [] } sessionProps) []

--------------------------------------------------------------

type ContactProps = ( annuaireId :: NodeID | SessionNodeProps )

contact :: R2.Component ContactProps
contact = R.createElement contactCpt
contactCpt :: R.Component ContactProps
contactCpt = here.component "contact" cpt where
  cpt props@{ annuaireId
            , nodeId
            } _ = do
    let
      sessionProps = (RE.pick props :: Record SessionProps)

      authedProps =
        { content:
            \_ -> contactLayout
                  { nodeId
                  , annuaireId
                  , key: "annuaireId-" <> show annuaireId
                  }
        } `Record.merge` sessionProps

    pure $ authed authedProps []
