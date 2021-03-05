module Gargantext.Components.Router (router) where

import Data.Array (fromFoldable)
import Data.Maybe (Maybe(..), maybe')
import Reactix as R
import Toestand as T
import Unsafe.Coerce (unsafeCoerce)

import Gargantext.Prelude

import Gargantext.AsyncTasks as GAT
import Gargantext.Components.App.Data (Cursors)
import Gargantext.Components.Footer (footer)
import Gargantext.Components.Forest (forestLayout, forestLayoutWithTopBar)
import Gargantext.Components.GraphExplorer (explorerLayout)
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
import Gargantext.Config (defaultFrontends, defaultBackends, publicBackend)
import Gargantext.Routes (AppRoute(..))
import Gargantext.Routes as GR
import Gargantext.Types (CorpusId, ListId, NodeID, NodeType(..), SessionId(..))
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2

here :: R2.Here
here = R2.here "Gargantext.Components.Router"

type Props = (
    cursors :: Cursors
  , tasks   :: T.Cursor (Maybe GAT.Reductor)
  , views   :: App.Views
  )

router :: R2.Leaf Props
router props = R.createElement routerCpt props []

routerCpt :: R.Component Props
routerCpt = here.component "root" cpt where
  cpt props@{ cursors, views, tasks } _ = do
    let session = R.createContext (unsafeCoerce {})
    showLogin <- T.useLive T.unequal views.showLogin
    route <- T.useLive (T.changed notEq) views.route
    if showLogin then login' cursors views
    else case route of
      GR.Annuaire s n           -> annuaire props s n
      GR.Corpus s n             -> corpus props s n
      GR.CorpusDocument s c l n -> corpusDocument props s c l n
      GR.Dashboard s n          -> dashboard props s n
      GR.Document s l n         -> document props s l n
      GR.Folder        s n      -> corpus props s n
      GR.FolderPrivate s n      -> corpus props s n
      GR.FolderPublic  s n      -> corpus props s n
      GR.FolderShared  s n      -> corpus props s n
      GR.Home                   -> home props
      GR.Lists s n              -> lists props s n
      GR.Login                  -> login' cursors
      GR.PGraphExplorer s g     -> graphExplorer props s g
      GR.RouteFile s n          -> routeFile props s n
      GR.RouteFrameCalc  s n    -> routeFrame props s n NodeFrameCalc
      GR.RouteFrameCode  s n    -> routeFrame props s n NodeFrameNotebook
      GR.RouteFrameWrite s n    -> routeFrame props s n NodeFrameWrite
      GR.Team s n               -> team props s n
      GR.Texts s n              -> texts props s n
      GR.UserPage s n           -> user props s n
      GR.ContactPage s a n      -> contact props s a n
 
forested :: Record Props -> Array R.Element -> R.Element
forested { tasks, views: { route, handed, sessions }
         , cursors: { backend, reloadForest, reloadRoot, showLogin } } =
  forestLayout
  { tasks, frontends, route, handed, sessions
  , backend, reloadForest, reloadRoot, showLogin
  } where frontends = defaultFrontends

authed :: Record Props -> SessionId -> R.Element -> R.Element
authed props@{ cursors: { session }, views: views@{ sessions }
             , tasks } sessionId content =
  sessionWrapper { sessionId, session, sessions, fallback: home props }
  [ content, footer { session: views.session } ]

annuaire :: Record Props -> SessionId -> NodeID -> R.Element
annuaire props@{ tasks, cursors, views: { session } } sessionId nodeId =
  authed props sessionId $
    forested props [ annuaireLayout { nodeId, frontends, session } ]
    where frontends = defaultFrontends

corpus :: Record Props -> SessionId -> NodeID -> R.Element
corpus props@{ tasks, cursors, views } sessionId nodeId =
  authed props sessionId $
    forested props
    [ corpusLayout { nodeId, session: views.session  } ]

corpusDocument :: Record Props -> SessionId -> CorpusId -> ListId -> NodeID -> R.Element
corpusDocument props@{ tasks, cursors, views } sessionId corpusId' listId nodeId =
  authed props sessionId $
    forested props
    [ documentMainLayout { listId, nodeId, corpusId, sessionId, session: views.session } [] ]
    where corpusId = Just corpusId'

dashboard :: Record Props -> SessionId -> NodeID -> R.Element
dashboard props@{ tasks, cursors, views: { session } } sessionId nodeId =
  authed props sessionId $
    forested props [ dashboardLayout { nodeId, session } [] ]

document :: Record Props -> SessionId -> ListId -> NodeID -> R.Element
document props@{ tasks, cursors, views: { session } } sessionId listId nodeId =
  authed props sessionId $
    forested props
    [ documentMainLayout { listId, nodeId, corpusId, session } [] ]
    where corpusId = Nothing

home :: Record Props -> R.Element
home props@{ cursors: { backend, showLogin }, views: { sessions } } =
  forested props [ homeLayout  { sessions, backend, showLogin, lang: LL_EN } ]

lists :: Record Props -> SessionId -> NodeID -> R.Element
lists props@{ tasks
            , cursors: { reloadForest, reloadRoot, session, showLogin }
            , views: { backend, route, handed, sessions } } sessionId nodeId =
  authed props sessionId $
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
    , listsProps: { tasks, reloadRoot, reloadForest, nodeId, session }
    } []
    where frontends = defaultFrontends

login' :: Cursors -> R.Element
login' { backend, sessions, showLogin: visible } =
  login { backend, sessions, visible
        , backends: fromFoldable defaultBackends }

graphExplorer :: Record Props -> SessionId -> Int -> R.Element
graphExplorer props@{ views: { backend, route, handed, session, sessions }
                    , tasks, cursors: { showLogin } } sessionId graphId =
  authed props sessionId $
    simpleLayout { handed }
    [ explorerLayout { tasks, graphId, backend, route, frontends
                     , handed, session, sessions, showLogin } ]
    where frontends = defaultFrontends

routeFile :: Record Props -> SessionId -> NodeID -> R.Element
routeFile props@{ views: { session } } sessionId nodeId =
  authed props sessionId $ forested props [ fileLayout { nodeId, session } ]

routeFrame :: Record Props -> SessionId -> NodeID -> NodeType -> R.Element
routeFrame props@{ views: { session } } sessionId nodeId nodeType =
  authed props sessionId $ forested props [ frameLayout { nodeId, nodeType, session } ]

team :: Record Props -> SessionId -> NodeID -> R.Element
team props@{ tasks, cursors, views: { session }  } sessionId nodeId =
  authed props sessionId $ forested props [ corpusLayout { nodeId, session } ]

texts :: Record Props -> SessionId -> NodeID -> R.Element
texts props@{ cursors: { backend, reloadForest, reloadRoot, showLogin }
            , views: { route, handed, session, sessions }
            , tasks } sessionId nodeId =
  authed props sessionId $
    Texts.textsWithForest
    { forestProps: { frontends, tasks, route, handed, sessions
                   , backend, reloadForest, reloadRoot, showLogin }
    , textsProps: { frontends, nodeId, session } }
    [] where frontends = defaultFrontends

user :: Record Props -> SessionId -> NodeID -> R.Element
user props@{ cursors: { reloadRoot }, tasks, views } sessionId nodeId =
  authed props sessionId $
    forested props
    [ userLayout { tasks, nodeId, session: views.session, reloadRoot, frontends } ]
    where frontends = defaultFrontends

contact :: Record Props -> SessionId -> NodeID -> R.Element
contact props@{ tasks, cursors: { reloadRoot } } sessionId annuaireId nodeId =
  authed props sessionId $
    forested props
    [ contactLayout { annuaireId, tasks, nodeId, reloadRoot, frontends } ]
    where frontends = defaultFrontends
