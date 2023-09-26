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
import Gargantext.Components.GraphQL.Tree (BreadcrumbInfo)
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

type Props = ( boxes :: Boxes )

component :: R2.Leaf Props
component = R2.leaf componentCpt
componentCpt :: R.Component Props
componentCpt = here.component "breadcrumb" cpt where
  cpt props@{ boxes: { session } } _ = do
    -- | States
    -- |
    -- ref <- R.useRef $ R.createContext Nothing
    -- context <- R.readRef ref
    -- session <- useSession
    -- session' :: Session
    session' <- T.useLive T.unequal session
    -- R.provideContext SessionContext.context session'

    -- | Effects
    -- |
    url <- R.unsafeHooksEffect GU.href

    -- | Behaviors
    -- |
    let
      -- context = R.readRef ref
      currentUrl = url
      -- fragment = Fragment.toString $ Fragment.fromString "http://localhost:8008/#/corpus/user1@localhost:8008/112"
      -- fragment = Fragment.toString $ Fragment.fromString url
      currentNodeId = fromMaybe 0 $ fromString $ getLastUrlElement currentUrl
      -- getBreadcrumbData = 
      --     case session' of 
      --       Nothing -> Nothing
      --       Just _ -> getBreadcrumb $ fromMaybe Session { backend = ""
      --                                                   , caches = Map.empty
      --                                                   , token = ""
      --                                                   , treeId = 0
      --                                                   , userName = ""
      --                                                   , userId = 0 } $ session' currentNodeId
    
    case session' of 
      Nothing -> pure $ H.div {} []
      Just (session') -> do
        -- let breadcrumbData = getBreadcrumb session' currentNodeId
        pure $
          -- breadcrumb layout
          H.div {}
          [
          --   H.nav {} [
          --     breadcrumbView { boxes }
          --   ]
          -- ,
            H.nav 
            { className: "breadcrumb-wrapper bg-light"
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
              , H.li { className: "breadcrum-item" }
                [
                  H.span {}
                  [
                    H.text $ show session' <> " - " <> show currentNodeId
                    -- H.text $ show breadcrumbData
                  ]
                ]
              , 
                H.li { className: "breadcrumb-item" }
                [
                  H.a { href: "/" } 
                  [ H.span { className: "" }
                    [ 
                      B.icon { name: "folder-open-o" } 
                    ]
                  , H.text "parent folder" 
                  ]
                ]
              , 
                H.li { className: "breadcrumb-item" }
                [ H.span { className: "active-page" }
                  [ H.span { className: "" }
                    [ 
                      B.icon { name: "book" } 
                    ]
                  , H.text "current node"
                  ]
                ]
              ]
            ]
          ]
        -- where
        --   breadcrumbData = getBreadcrumbData





breadcrumbView :: R2.Leaf Props
breadcrumbView = R2.leaf breadcrumbViewCpt
breadcrumbViewCpt :: R.Component Props
breadcrumbViewCpt = here.component "breadcrumbViewCpt" cpt where
  cpt props@{ boxes: { session } } _ = do
    session' <- T.useLive T.unequal session
    url <- R.unsafeHooksEffect GU.href
    let nodeId = fromMaybe 0 $ fromString $ getLastUrlElement url

    case session' of 
      Nothing -> pure $ H.div {} []
      Just (session') -> do
        reload <- T.useBox T2.newReload
        reload' <- T.useLive T.unequal reload
        useLoader { errorHandler
                  , loader: loadBreadcrumbData
                  , path: { nodeId, session', reload: reload'}
                  , render: \items -> breadcrumbViewMain { items
                                                         , nodeId
                                                         , reload
                                                         , session'
                                                         } [] }
            where
          errorHandler = logRESTError here "[breadcrumbView]"

type BreadcrumbViewProps =
  ( items         :: BreadcrumbInfo
  , nodeId        :: Int
  , reload        :: T.Box T2.Reload
  , session       :: Session
  )

breadcrumbViewMain :: R2.Component BreadcrumbViewProps
breadcrumbViewMain = R.createElement breadcrumbViewMainCpt
breadcrumbViewMainCpt :: R.Component BreadcrumbViewProps
breadcrumbViewMainCpt = here.component "breadcrumbViewMainCpt" cpt where
  cpt props@{ items: { parents } } _ = do
    let breadcrumbItems = makeBreadcrumbItems parents
    -- let breadcrumbItems = pure $ H.text "breadcrumb in devlopment"

    pure $

      H.div
      { className: "breadcrumb-view" } $
      breadcrumbItems

  makeBreadcrumbItems :: Array TreeNode -> Maybe TreeNode -> Record BreadcrumbViewProps -> Array R.Element
  makeBreadcrumbItems root (Just parent) props =
    [
      breadcrumbItem
      { name: parent.name
      , parent_id: parent.parent_id
      , node_type: parent.node_type
      , id: parent.id
      }
    ]
    -- where
    --   disabled { node_type } = if node_type == GT.FolderShared then true else false
  makeBreadcrumbItems _ Nothing _ = []

type BreadcrumbItemProps = 
  ( node_type       :: NodeType
  , id              :: Int
  , parent_id       :: Int
  , name            :: String
  )

breadcrumbItem :: R2.Leaf BreadcrumbItemProps
breadcrumbItem = R2.leaf breadcrumbItemCpt
breadcrumbItemCpt :: R.Component BreadcrumbItemProps
breadcrumbItemCpt = here.component "breadcrumbItemCpt" cpt where
  cpt props@{ nodeType
            , nodeId
            , parentId
            , reload
            , session
            , style
            , text
            } _ = do
    pure $ H.div {} []

getLastUrlElement :: String -> String
getLastUrlElement str = fromMaybe "" $ A.last $ split (Pattern "/") str

type LoadProps =
  (
    session :: Session,
    nodeId :: Int,
    reload :: T2.Reload
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
