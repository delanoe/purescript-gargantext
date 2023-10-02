module Gargantext.Components.Forest.Breadcrumb (component) where

import Prelude

import Control.Monad.Trans.Class
import Data.String (Pattern(..), split)
import Data.Array as A
import Data.Int (fromString)
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
import Gargantext.Sessions
import Gargantext.Sessions.Types
import Gargantext.Types (CorpusId, FrontendError, NodeID, NodeType, SessionId)
import Gargantext.Utils as GU
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import URI.Query as Query
import URI.Fragment as Fragment
import Effect (Effect)
import Effect.Console (log)
import Toestand as T
import Gargantext.Config.REST (AffRESTError, logRESTError)
import Gargantext.Utils.Toestand as T2
import Gargantext.Hooks.Loader (useLoader)



here :: R2.Here
here = R2.here "Gargantext.Components.Forest.Breadcrumb"

type PropsBoxes = ( boxes :: Boxes )
type Props =
  ( nodeId     :: Int
  , session    :: Maybe Session
  )

maybeToSession :: Maybe Session -> Session
maybeToSession s = fromMaybe Nothing s


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
    url <- R.unsafeHooksEffect GU.href

    -- | Behaviors
    -- |
    let
      -- currentUrl = url
      -- fragment = Fragment.toString $ Fragment.fromString "http://localhost:8008/#/corpus/user1@localhost:8008/112"
      -- fragment = Fragment.toString $ Fragment.fromString url
      currentNodeId = fromMaybe 0 $ fromString $ getLastUrlElement url

    
    case session' of 
      Nothing -> pure $ H.div {} []
      Just (session') -> do
        
        -- breadcrumbData <- R2.useLayoutEffect1' $ getBreadcrumb session' currentNodeId

        pure $
          -- breadcrumb layout
          H.div {}
          [
            breadcrumbView { currentNodeId, session' }
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
breadcrumbViewCpt = here.component "breadcrumbViewCpt" cpt where
  cpt { nodeId, session } _ = do
    -- session' <- session
    -- session' <- T.useLive T.unequal session
    -- url <- R.unsafeHooksEffect GU.href
    -- let nodeId = fromMaybe 0 $ fromString $ getLastUrlElement url
    session' <- liftEffect $ maybeToSession session

    case session' of 
      Nothing -> pure $ H.div {} []
      Just (session') -> do
        -- reload <- T.useBox T2.newReload
        -- reload' <- T.useLive T.unequal reload
        useLoader { errorHandler
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
        where
          errorHandler = logRESTError here "[breadcrumbView]"

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
  cpt props@{ items: { parents } } _ = do
    let items = makeBreadcrumbElements parents props

    pure $  
      H.div { className: "breadcrumb-view" } $
      items      


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
                                                    -- , style: FolderChild
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
    pure $ 
    
      H.div { className: "bItem" } 
      [
        H.text props.text
      ]

getLastUrlElement :: String -> String
getLastUrlElement str = fromMaybe "" $ A.last $ split (Pattern "/") str

type LoadProps =
  (
    session :: Session
  , nodeId :: Int
  -- , reload :: T.Box T2.Reload
  )

loadBreadcrumbData :: Record LoadProps -> AffRESTError BreadcrumbInfo
loadBreadcrumbData {nodeId, session} = getBreadcrumb session nodeId




-- type BreadcrumbProps = ( nodeId  :: NodeID )


-- breadcrumbLayout :: R2.Leaf BreadcrumbProps
-- breadcrumbLayout = R2.leaf breadcrumbLayoutCpt
-- breadcrumbLayoutCpt :: R.Component BreadcrumbProps
-- breadcrumbLayoutCpt = here.component "breadcrumbLayout" cpt where
--   cpt props@{ nodeId } _ = do

--     session <- useSession

--     let
--       breadcrumbData = getBreadcrumb session nodeId

--     pure $ 
--       -- breadcrumb layout
--       H.div {}
--       [
--         H.nav 
--         { className: "breadcrumb-wrapper bg-light"
--         , "aria-label": "breadcrumb" }
--         [
--           H.ol { className: "breadcrumb text-small" }
--           [
--             H.li { className: "breadcrumb-item" }
--             [
--               H.a { href: "/" } 
--               [ H.span { className: "" }
--                 [ 
--                   B.icon { name: "home" } 
--                 ]
--               , H.text "Home" 
--               ]
--             ]
--           , 
--             H.li { className: "breadcrumb-item" }
--             [
--               H.a { href: "/" } 
--               [ H.span { className: "" }
--                 [ 
--                   B.icon { name: "folder-open-o" } 
--                 ]
--               , H.text "parent folder" 
--               ]
--             ]
--           , 
--             H.li { className: "breadcrumb-item" }
--             [ H.span { className: "active-page" }
--               [ H.span { className: "" }
--                 [ 
--                   B.icon { name: "book" } 
--                 ]
--               , H.text "current node"
--               ]
--             ]
--           ]
--         ]
--       ]
