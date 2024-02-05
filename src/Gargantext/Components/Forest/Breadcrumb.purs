module Gargantext.Components.Forest.Breadcrumb (component) where

import Prelude

import Control.Monad.Trans.Class
import Data.String (Pattern(..), split)
import Data.Array as A
import Data.Int (fromString)
import Data.List (head)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, throwError)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (error)
import Gargantext.Prelude
import Gargantext.Context.Session as SessionContext
import Gargantext.Components.App.Store (Boxes)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.GraphQL.Endpoints (getBreadcrumb)
import Gargantext.Components.GraphQL.Tree (BreadcrumbInfo, TreeNode)
import Gargantext.Components.Login.Types (TreeId, UserId)
import Gargantext.Ends (Frontends, Backend(..))
import Gargantext.Hooks.Session (useSession)
import Gargantext.Routes (AppRoute(Home), appPath, nodeTypeAppRoute)
import Gargantext.Sessions
import Gargantext.Sessions.Types
import Gargantext.Types (CorpusId, FrontendError, NodeID, NodeType, SessionId)
import Gargantext.Types as GT
import Gargantext.Utils as GU
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import URI.Query as Query
import URI.Fragment as Fragment
import Effect (Effect)
import Effect.Console (log)
import Toestand as T
import Gargantext.Config.REST (AffRESTError)
import Gargantext.Utils.Toestand as T2
import Gargantext.Hooks.Loader (useLoader)



here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Breadcrumb"

type PropsBoxes = ( boxes :: Boxes )

type Props =
  ( nodeId     :: Int
  , session    :: Maybe Session
  )

-- maybeToSession :: Maybe Session -> Session
-- maybeToSession s = fromMaybe Nothing s


component :: R2.Leaf PropsBoxes
component = R2.leaf componentCpt
componentCpt :: R.Component PropsBoxes
componentCpt = here.component "breadcrumb" cpt where
  cpt props@{ boxes: { session } } _ = do
    -- | States
    -- |
    session' <- T.useLive T.unequal session
    -- R.provideContext SessionContext.context session'

    -- | Effects
    -- |
    -- url <- R.unsafeHooksEffect GU.href

    -- | Behaviors
    -- |
    -- let
      -- currentUrl = url
      -- fragment = Fragment.toString $ Fragment.fromString "http://localhost:8008/#/corpus/user1@localhost:8008/112"
      -- fragment = Fragment.toString $ Fragment.fromString url
      -- currentNodeId = fromMaybe 0 $ fromString $ getLastUrlElement url

    
    case session' of 
      Nothing -> pure $ H.div {} []
      Just (session') -> do
        
        url <- R.unsafeHooksEffect GU.href
        let nodeId = fromMaybe 0 $ fromString $ getLastUrlElement url
        -- breadcrumbData <- R2.useLayoutEffect1' $ getBreadcrumb session' currentNodeId
        let session'' = Just session'

        pure $
          -- breadcrumb layout
          H.nav { className: "breadcrumb-wrapper bg-light"
                , "aria-label": "breadcrumb" }
          [
            H.ol { className: "breadcrumb text-small" }
            [
              H.li { className: "breadcrumb-item" }
              [
                H.a { href: "/" } 
                [ H.span { className: "" }
                  [ 
                    B.icon { name: "home" }
                  ]
                , H.text "Home" 
                ]
              ]
            ,
              breadcrumbView { nodeId: nodeId
                              , session: Just session' 
                              }
            ]
          -- ,
          --   H.nav 
          --   { className: "breadcrumb-wrapper bg-light"
          --   , "aria-label": "breadcrumb" }
          --   [
          --     H.ol { className: "breadcrumb text-small" }
          --     [
          --       H.li { className: "breadcrumb-item" }
          --       [
          --         H.a { href: "/" } 
          --         [ H.span { className: "" }
          --           [ 
          --             B.icon { name: "home" }
          --           ]
          --         , H.text "Home" 
          --         ]
          --       ]
          --     , H.li { className: "breadcrum-item" }
          --       [
          --         H.span {}
          --         [
          --           H.text $ show session' <> " - " <> show currentNodeId
          --           -- H.text $ show breadcrumbData
          --         ]
          --       ]
          --     , 
          --       H.li { className: "breadcrumb-item" }
          --       [
          --         H.a { href: "/" } 
          --         [ H.span { className: "" }
          --           [ 
          --             B.icon { name: "folder-open-o" } 
          --           ]
          --         , H.text "parent folder" 
          --         ]
          --       ]
          --     , 
          --       H.li { className: "breadcrumb-item" }
          --       [ H.span { className: "active-page" }
          --         [ H.span { className: "" }
          --           [ 
          --             B.icon { name: "book" } 
          --           ]
          --         , H.text "current node"
          --         ]
          --       ]
          --     ]
          --   ]
          ]


breadcrumbView :: R2.Leaf Props
breadcrumbView = R2.leaf breadcrumbViewCpt
breadcrumbViewCpt :: R.Component Props
breadcrumbViewCpt = R2.hereComponent here "breadcrumbViewCpt" hCpt where
  hCpt hp { nodeId, session } _ = do
    let session' = session

    case session' of 
      Nothing -> pure $ H.div {} []
      Just (session') -> do
        useLoader { errorHandler: Nothing
                  , herePrefix: hp
                  , loader: loadBreadcrumbData
                  , path: { nodeId: nodeId
                          , session: session'
                          -- , reload: reload'
                          }
                  , render: \items -> breadcrumbViewMain { items: items
                                                         , nodeId: nodeId
                                                         , session: session'
                                                        --  , reload: reload
                                                         } [] 
                  }

type BreadcrumbViewProps =
  ( items         :: BreadcrumbInfo
  , nodeId        :: Int
  -- , reload        :: T.Box T2.Reload
  , session       :: Session
  )

breadcrumbViewMain :: R2.Component BreadcrumbViewProps
breadcrumbViewMain = R.createElement breadcrumbViewMainCpt
breadcrumbViewMainCpt :: R.Component BreadcrumbViewProps
breadcrumbViewMainCpt = here.component "breadcrumbViewMainCpt" cpt where
  cpt props@{ items: { parents }, session } _ = do
    
    -- session' <- T.useLive T.unequal session
    let items = makeBreadcrumbElements parents props

    -- case session of 
    --   Nothing -> pure $ H.div {} []
    --   Just (session) -> do

    pure $ 
      R.fragment items


  makeBreadcrumbElements :: Array TreeNode -> Record BreadcrumbViewProps -> Array R.Element
  makeBreadcrumbElements items' props = makeBreadcrumbElementsMap <$> items' where
    makeBreadcrumbElementsMap :: TreeNode -> R.Element
    makeBreadcrumbElementsMap node = breadcrumbItem { nodeId: node.id
                                                    , linkId: node.id
                                                    , nodeType: node.node_type
                                                    , linkNodeType: node.node_type
                                                    , parentId: props.nodeId
                                                    -- , reload: props.reload
                                                    , session: props.session
                                                    -- , style: FolderUp
                                                    , text: node.name
                                                    , disabled: false
                                                    }

type BreadcrumbItemProps =
  ( disabled      :: Boolean
  , linkNodeType  :: NodeType
  , linkId        :: Int
  , nodeType      :: NodeType
  , nodeId        :: Int
  , parentId      :: Int
  -- , reload        :: T.Box T2.Reload
  , session       :: Session
  -- , style         :: FolderStyle
  , text          :: String
  )

breadcrumbItem :: R2.Leaf BreadcrumbItemProps
breadcrumbItem = R2.leaf breadcrumbItemCpt
breadcrumbItemCpt :: R.Component BreadcrumbItemProps
breadcrumbItemCpt = here.component "breadcrumbItemCpt" cpt where
  cpt props@{ nodeId
            , linkId
            , nodeType
            , linkNodeType
            , parentId
            -- , reload
            , session
            -- , style
            , text
            , disabled
            } _ = do

    url <- R.unsafeHooksEffect GU.href

    let sid = sessionId session
    let rootId = treeId session

    pure $ 
      H.li { className: "breadcrumb-item" }
      [
        if show nodeType == "NodeFolderPrivate" 
        || show nodeType == "NodeFolderPublic" 
        || show nodeType == "NodeFolderShared" 
        || show nodeType == "NodeUser" 
        then
          H.span { className: "" }
          [
            H.span { className: "" }
            [ 
              B.icon
              { className: ""
              , name: GT.getIcon nodeType true
              }
            ]
          , 
            if show nodeType == "NodeUser"
            then
              H.text $ getUserText url 
            else
              H.text props.text
          
          , H.span { className: "text-small" }
            [ 
            if show nodeType == "NodeUser"
            then
              H.text $ " (" <> getInstanceText url <> ")"
            else
              H.text ""
            ]
        
          ]

        else
          H.a { className: ""
              , href: "/#/" <> createNodeUrl linkId rootId linkNodeType sid 
              } 
          [ 
            H.span { className: "" }
            [ 
              -- B.icon { name: "folder-open-o" } 
              B.icon
              { className: ""
              , name: GT.getIcon nodeType true
              }
            ]
          , 
            H.text props.text 
          ]
      ]

  createNodeUrl :: Int -> Int -> NodeType -> SessionId -> String
  createNodeUrl lId rootId nType sId
    -- | rootId == lId  = appPath Home
    | otherwise      = appPath $ getFolderPath nType sId lId
  
  getFolderPath :: GT.NodeType -> GT.SessionId -> Int -> AppRoute
  getFolderPath nodeType sid nodeId = fromMaybe Home $ nodeTypeAppRoute nodeType sid nodeId


treeId :: Session -> Int
treeId (Session {treeId: tId}) = tId

getLastUrlElement :: String -> String
getLastUrlElement str = fromMaybe "" $ A.last $ split (Pattern "/") str

getFirstUrlElement :: String -> String
getFirstUrlElement str = fromMaybe "" $ A.head $ split (Pattern "/") str

getInstanceText :: String -> String
getInstanceText str = getFirstUrlElement $ fromMaybe "" $ A.last $ split (Pattern "@") str

getUserText :: String -> String
getUserText str = getLastUrlElement $ fromMaybe "" $ A.head $ split (Pattern "@") str


type LoadProps =
  (
    session :: Session
  , nodeId :: Int
  -- , reload :: T.Box T2.Reload
  )

loadBreadcrumbData :: Record LoadProps -> AffRESTError BreadcrumbInfo
loadBreadcrumbData {nodeId, session} = getBreadcrumb session nodeId
