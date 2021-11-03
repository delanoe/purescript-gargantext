module Gargantext.Components.Nodes.Annuaire.User
  ( module Gargantext.Components.Nodes.Annuaire.User.Contacts.Types
  , userLayout
  )
  where


import Gargantext.Components.GraphQL.User
import Gargantext.Prelude

import Data.Either (Either)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Gargantext.Components.App.Data (Boxes)
import Gargantext.Components.Nodes.Annuaire.Tabs as Tabs
import Gargantext.Components.Nodes.Annuaire.User.Contact (getUserInfoWithReload, saveUserInfo, contactInfos)
import Gargantext.Components.Nodes.Annuaire.User.Contacts.Types (Contact(..), ContactData, ContactTouch(..), ContactWhere(..), ContactWho(..), HyperdataContact(..), HyperdataUser(..), _city, _country, _firstName, _labTeamDeptsJoinComma, _lastName, _mail, _office, _organizationJoinComma, _ouFirst, _phone, _role, _shared, _touch, _who, defaultContactTouch, defaultContactWhere, defaultContactWho, defaultHyperdataContact, defaultHyperdataUser)
import Gargantext.Components.Nodes.Lists.Types as LT
import Gargantext.Config.REST (RESTError, logRESTError)
import Gargantext.Config.Utils (handleRESTError)
import Gargantext.Ends (Frontends)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes as Routes
import Gargantext.Sessions (WithSession, WithSessionContext, Session, get, put, sessionId)
import Gargantext.Types (FrontendError, NodeType(..))
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Annuaire.User"

type DisplayProps = ( title :: String )

display :: R2.Component DisplayProps
display = R.createElement displayCpt
displayCpt :: R.Component DisplayProps
displayCpt = here.component "display" cpt
  where
    cpt { title } children = do
      pure $ H.div { className: "container-fluid" }
        [ H.div { className: "row", id: "contact-page-header" }
          [ H.div { className: "col-md-6"} [ H.h3 {} [ H.text title ] ]
          , H.div { className: "col-md-8"} []
          , H.div { className: "col-md-2"} [ H.span {} [ H.text "" ] ]
          ]
        , H.div { className: "row", id: "contact-page-info" }
          [ H.div { className: "col-md-12" }
            [ H.div { className: "row" }
              [ H.div { className: "col-md-2" } [ H.img { src: "/images/Gargantextuel-212x300.jpg"} ]
              , H.div { className: "col-md-1"} []
              , H.div { className: "col-md-8"} children
              ]
            ]
          ]
        ]

{-
listElement :: Array R.Element -> R.Element
listElement = H.li { className: "list-group-item justify-content-between" }
-}

type LayoutNoSessionProps =
  ( boxes     :: Boxes
  , frontends :: Frontends
  , nodeId    :: Int
  )

type LayoutProps = WithSession LayoutNoSessionProps

type LayoutSessionContextProps = WithSessionContext LayoutNoSessionProps

type KeyLayoutProps = (
    key :: String
  | LayoutProps
  )

userLayout :: R2.Component LayoutProps
userLayout = R.createElement userLayoutCpt
userLayoutCpt :: R.Component LayoutProps
userLayoutCpt = here.component "userLayout" cpt
  where
    cpt props@{ nodeId
              , session } _ = do
      let sid = sessionId session

      pure $ userLayoutWithKey $ Record.merge props { key: show sid <> "-" <> show nodeId }

userLayoutWithKey :: R2.Leaf KeyLayoutProps
userLayoutWithKey props = R.createElement userLayoutWithKeyCpt props []
userLayoutWithKeyCpt :: R.Component KeyLayoutProps
userLayoutWithKeyCpt = here.component "userLayoutWithKey" cpt where
  cpt { boxes: boxes@{ sidePanelTexts }
      , frontends
      , nodeId
      , session } _ = do
    reload <- T.useBox T2.newReload
    reload' <- T.useLive T.unequal reload

    cacheState <- T.useBox LT.CacheOn

    useLoader { errorHandler
              , loader: getUserInfoWithReload
              , path: { nodeId, reload: reload', session }
              , render: \userInfo@{ ui_username } ->
                  H.ul { className: "col-md-12 list-group" } [
                    display { title: fromMaybe "no name" (Just ui_username) }
                    (contactInfos userInfo (onUpdateUserInfo boxes.errors reload))
                    , Tabs.tabs {
                         boxes
                       , cacheState
                       , defaultListId: 424242
                       , frontends
                       , nodeId
                       , session
                       , sidePanel: sidePanelTexts
                       }
                    ]
              }
    where
      errorHandler = logRESTError here "[userLayoutWithKey]"
      onUpdateUserInfo :: T.Box (Array FrontendError) -> T2.ReloadS -> UserInfo -> Effect Unit
      onUpdateUserInfo errors reload ui = do
        launchAff_ $ do
          res <- saveUserInfo session nodeId ui
          handleRESTError errors res $ \_ ->
            liftEffect $ T2.reload reload

saveContactHyperdata :: Session -> Int -> HyperdataUser -> Aff (Either RESTError Int)
saveContactHyperdata session id = put session (Routes.NodeAPI Node (Just id) "")

-- | toUrl to get data XXX
getContact :: Session -> Int -> Aff (Either RESTError ContactData)
getContact session id = do
  eContactNode <- get session $ Routes.NodeAPI Node (Just id) ""
  -- TODO: we need a default list for the pairings
  --defaultListIds <- get $ toUrl endConfigStateful Back (Children NodeList 0 1 Nothing) $ Just id
  --case (head defaultListIds :: Maybe (NodePoly HyperdataList)) of
  --  Just (NodePoly { id: defaultListId }) ->
  --    pure {contactNode, defaultListId}
  --  Nothing ->
  --    throwError $ error "Missing default list"
  pure $ (\contactNode -> { contactNode, defaultListId: 424242 }) <$> eContactNode
