module Gargantext.Components.App where

import Data.Array (fromFoldable)
import Data.Maybe (Maybe(..), maybe')
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Reactix as R

import Gargantext.Prelude

import Gargantext.Components.Footer (footer)
import Gargantext.Components.Forest (forestLayout, forestLayoutWithTopBar)
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
import Gargantext.Components.Nodes.Texts as Texts
import Gargantext.Components.SimpleLayout (simpleLayout)
import Gargantext.Config (defaultFrontends, defaultBackends, publicBackend)
import Gargantext.Hooks.Router (useHashRouter)
import Gargantext.Router (router)
import Gargantext.Routes (AppRoute(..))
import Gargantext.Sessions (useSessions)
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

    asyncTasksRef <- R.useRef Nothing
    treeReloadRef <- R.useRef Nothing

    showLogin  <- R.useState' false
    backend    <- R.useState' Nothing

    appReload  <- R.useState' 0

    showCorpus <- R.useState' false

    handed <- R.useState' GT.RightHanded

    let backends          = fromFoldable defaultBackends
    let ff f session      = R.fragment [ f session, footer { session } ]
    let forested = forestLayout { appReload
                                , asyncTasksRef
                                , backend
                                , frontends
                                , handed
                                , route: fst route
                                , sessions: fst sessions
                                , showLogin: snd showLogin
                                , treeReloadRef
                                }
    let forestedTB = forestLayoutWithTopBar { appReload
                                            , asyncTasksRef
                                            , backend
                                            , frontends
                                            , handed
                                            , route: fst route
                                            , sessions: fst sessions
                                            , showLogin: snd showLogin
                                            , treeReloadRef
                                            }
    let defaultView _ = forested [
          homeLayout { backend
                     , lang: LL_EN
                     , publicBackend
                     , sessions
                     , visible: showLogin
                     }
          ]
    let mCurrentRoute     = fst route
    let withSession sid f = maybe' defaultView (ff f) (Sessions.lookup sid (fst sessions))

    let sessionUpdate s = snd sessions $ Sessions.Update s

    pure $ case fst showLogin of
      true -> forested [ login { backend, backends, sessions, visible: showLogin } ]
      false ->
        case fst route of
          Annuaire sid nodeId        -> withSession sid $ \session -> forested [
            annuaireLayout { frontends, nodeId, session }
          ]
          ContactPage sid aId nodeId                -> withSession sid $ \session -> forested [
            annuaireUserLayout {
                annuaireId: aId
              , appReload
              , asyncTasksRef
              , frontends
              , nodeId
              , session
              , treeReloadRef
              }
            ]
          Corpus sid nodeId        -> withSession sid $ \session -> forested [
            corpusLayout { nodeId, session }
          ]
          CorpusDocument sid corpusId listId nodeId -> withSession sid $ \session -> forested [
            documentLayout { listId, mCorpusId: Just corpusId,  nodeId, session } []
          ]
          Dashboard sid nodeId       -> withSession sid $ \session -> forested [
            dashboardLayout { nodeId, session }
          ]
          Document sid listId nodeId ->
            withSession sid $
              \session -> forested [
                documentLayout { listId, mCorpusId: Nothing, nodeId, session } []
              ]
          Folder sid nodeId -> withSession sid $ \session -> forested [ corpusLayout { nodeId, session } ]
          FolderPrivate sid nodeId -> withSession sid $ \session -> forested [ corpusLayout { nodeId, session } ]
          FolderPublic sid nodeId  -> withSession sid $ \session -> forested [ corpusLayout { nodeId, session } ]
          FolderShared sid nodeId  -> withSession sid $ \session -> forested [ corpusLayout { nodeId, session } ]
          Home  -> forested [
            homeLayout { backend, lang: LL_EN, publicBackend, sessions, visible: showLogin }
          ]
          Lists sid nodeId         -> withSession sid $
            \session -> forested [
              listsLayout {
                  appReload
                , asyncTasksRef
                , nodeId
                , session
                , sessionUpdate
                , treeReloadRef
                }
            ]
          Login -> login { backend, backends, sessions, visible: showLogin }
          PGraphExplorer sid graphId ->
            withSession sid $
              \session ->
                simpleLayout { handed } [
                  explorerLayout { asyncTasksRef
                                 , backend
                                 , frontends
                                 , graphId
                                 , handed: fst handed
                                 , mCurrentRoute
                                 , session
                                 , sessions: (fst sessions)
                                 , showLogin
                                 }
                ]
          RouteFile sid nodeId -> withSession sid $ \session -> forested [ fileLayout { nodeId, session } ]
          RouteFrameCalc  sid nodeId -> withSession sid $ \session -> forested [
            frameLayout { nodeId, nodeType: GT.NodeFrameCalc, session     }
          ]
          RouteFrameCode  sid nodeId -> withSession sid $ \session -> forested [
            frameLayout { nodeId, nodeType: GT.NodeFrameNotebook, session }
          ]
          RouteFrameWrite sid nodeId -> withSession sid $ \session -> forested [
            frameLayout { nodeId, nodeType: GT.NodeFrameWrite, session    }
          ]
          Team sid nodeId  -> withSession sid $ \session -> forested [
            corpusLayout { nodeId, session }
          ]
          Texts sid nodeId         -> withSession sid $
            \session -> Texts.textsWithForest {
                  forestProps: {
                      appReload
                    , asyncTasksRef
                    , backend
                    , frontends
                    , handed
                    , route: fst route
                    , sessions: fst sessions
                    , showLogin: snd showLogin
                    , treeReloadRef
                  }
                , textsProps: {
                      frontends
                    , nodeId
                    , session
                    , sessionUpdate
                }
              } []
          UserPage sid nodeId        -> withSession sid $ \session -> forested [
            userLayout {
                appReload
              , asyncTasksRef
              , frontends
              , nodeId
              , session
              , treeReloadRef
              }
          ]
