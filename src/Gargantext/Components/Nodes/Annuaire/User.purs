module Gargantext.Components.Nodes.Annuaire.User
  ( module Gargantext.Components.Nodes.Annuaire.User.Contacts.Types
  , userLayout
  )
  where


import Gargantext.Prelude

import Data.Either (Either(..))
import Data.Lens as L
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Gargantext.Components.App.Store (Boxes)
import Gargantext.Components.GraphQL (getClient)
import Gargantext.Components.GraphQL.Endpoints (getUserInfo)
import Gargantext.Components.GraphQL.User (UserInfo, _ui_cwCity, _ui_cwCountry, _ui_cwFirstName, _ui_cwLabTeamDeptsFirst, _ui_cwLastName, _ui_cwOffice, _ui_cwOrganizationFirst, _ui_cwRole, _ui_cwTouchMail, _ui_cwTouchPhone)
import Gargantext.Components.InputWithEnter (inputWithEnter)
import Gargantext.Components.Nodes.Annuaire.Tabs as Tabs
import Gargantext.Components.Nodes.Annuaire.User.Contacts.Types (Contact(..), ContactData, ContactTouch(..), ContactWhere(..), ContactWho(..), HyperdataContact(..), HyperdataUser(..), _city, _country, _firstName, _labTeamDeptsJoinComma, _lastName, _mail, _office, _organizationJoinComma, _ouFirst, _phone, _role, _shared, _touch, _who, defaultContactTouch, defaultContactWhere, defaultContactWho, defaultHyperdataContact, defaultHyperdataUser)
import Gargantext.Components.Nodes.Lists.Types as LT
import Gargantext.Config.REST (AffRESTError, logRESTError)
import Gargantext.Config.Utils (handleRESTError)
import Gargantext.Ends (Frontends)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Sessions (Session(..), WithSession, WithSessionContext, sessionId)
import Gargantext.Types (FrontendError)
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import GraphQL.Client.Args (IgnoreArg(..), OrArg(..), onlyArgs)
import GraphQL.Client.Query (mutation)
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

userLayout :: R2.Component LayoutProps
userLayout = R.createElement userLayoutCpt
userLayoutCpt :: R.Component LayoutProps
userLayoutCpt = here.component "userLayout" cpt
  where
    cpt props@{ nodeId
              , session } _ = do
      let sid = sessionId session

      pure $ userLayoutWithKey $ Record.merge props { key: show sid <> "-" <> show nodeId }

userLayoutWithKey :: R2.Leaf ( key :: String | LayoutProps )
userLayoutWithKey = R2.leafComponent userLayoutWithKeyCpt
userLayoutWithKeyCpt :: R.Component ( key :: String | LayoutProps )
userLayoutWithKeyCpt = here.component "userLayoutWithKey" cpt where
  cpt { boxes: boxes@{ sidePanelTexts }
      , frontends
      , nodeId
      , session } _ = do
    reload <- T.useBox T2.newReload
    reload' <- T.useLive T.unequal reload

    let Session {userId} = session

    cacheState <- T.useBox LT.CacheOn

    useLoader { errorHandler
              , loader: getUserInfoWithReload
              , path: { nodeId: userId, reload: reload', session }
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
          let Session {userId} = session
          res <- saveUserInfo session userId ui
          handleRESTError errors res $ \_ ->
            liftEffect $ T2.reload reload

--saveContactHyperdata :: Session -> Int -> HyperdataUser -> AffRESTError Int
--saveContactHyperdata session id = put session (Routes.NodeAPI Node (Just id) "")

-- | toUrl to get data XXX
--getContact :: Session -> Int -> AffRESTError ContactData
--getContact session id = do
--  eContactNode <- get session $ Routes.NodeAPI Node (Just id) ""
--  -- TODO: we need a default list for the pairings
--  --defaultListIds <- get $ toUrl endConfigStateful Back (Children NodeList 0 1 Nothing) $ Just id
--  --case (head defaultListIds :: Maybe (NodePoly HyperdataList)) of
--  --  Just (NodePoly { id: defaultListId }) ->
--  --    pure {contactNode, defaultListId}
--  --  Nothing ->
--  --    throwError $ error "Missing default list"
--  pure $ (\contactNode -> { contactNode, defaultListId: 424242 }) <$> eContactNode
--

------------------------------------------------------------

-- | TODO format data in better design (UI) shape
contactInfos :: UserInfo -> (UserInfo -> Effect Unit) -> Array R.Element
contactInfos userInfo onUpdateUserInfo = item <$> contactInfoItems where
  item { label, lens, defaultVal } =
    contactInfoItem { defaultVal, label, lens, onUpdateUserInfo, userInfo }

contactInfoItems :: Array {label:: String, defaultVal:: String, lens:: UserInfoLens}
contactInfoItems =
  [ { label: "Last Name"    , defaultVal: "Empty Last Name"    , lens: _ui_cwLastName                     }
  , { label: "First Name"   , defaultVal: "Empty First Name"   , lens: _ui_cwFirstName                    }
  , { label: "Organisation" , defaultVal: "Empty Organisation" , lens: _ui_cwOrganizationFirst            }
  , { label: "Lab/Team/Dept", defaultVal: "Empty Lab/Team/Dept", lens: _ui_cwLabTeamDeptsFirst            }
  , { label: "Office"       , defaultVal: "Empty Office"       , lens: _ui_cwOffice                       }
  , { label: "City"         , defaultVal: "Empty City"         , lens: _ui_cwCity                         }
  , { label: "Country"      , defaultVal: "Empty Country"      , lens: _ui_cwCountry                      }
  , { label: "Role"         , defaultVal: "Empty Role"         , lens: _ui_cwRole                         }
  , { label: "Phone"        , defaultVal: "Empty Phone"        , lens: _ui_cwTouchPhone                   }
  , { label: "Mail"         , defaultVal: "Empty Mail"         , lens: _ui_cwTouchMail                    }
  ]

type UserInfoLens = L.ALens' UserInfo String

type ContactInfoItemProps =
  ( defaultVal       :: String
  , label            :: String
  , lens             :: UserInfoLens
  , onUpdateUserInfo :: UserInfo -> Effect Unit
  , userInfo         :: UserInfo
  )

contactInfoItem :: R2.Leaf ContactInfoItemProps
contactInfoItem = R2.leafComponent contactInfoItemCpt
contactInfoItemCpt :: R.Component ContactInfoItemProps
contactInfoItemCpt = here.component "contactInfoItem" cpt
  where
    cpt { defaultVal, label, lens, onUpdateUserInfo, userInfo } _ = do
      isEditing <- T.useBox false
      isEditing' <- T.useLive T.unequal isEditing

      let value = (L.view cLens userInfo) :: String

      valueBox <- T.useBox value
      pure $
        H.div { className: "form-group row" }
          [ H.span { className: "col-sm-2 col-form-label" } [ H.text label ]
          , if isEditing' then
              itemEditing { defaultVal, isEditing, lens, onUpdateUserInfo, userInfo, valueBox }
            else
              itemNotEditing { defaultVal, isEditing, lens, onUpdateUserInfo, userInfo, valueBox }
          ]
      where
        cLens = L.cloneLens lens

type ItemProps =
  ( defaultVal       :: String
  , isEditing        :: T.Box Boolean
  , lens             :: UserInfoLens
  , onUpdateUserInfo :: UserInfo -> Effect Unit
  , userInfo         :: UserInfo
  , valueBox         :: T.Box String
  )

itemNotEditing :: R2.Leaf ItemProps
itemNotEditing = R2.leafComponent itemNotEditingCpt
itemNotEditingCpt :: R.Component ItemProps
itemNotEditingCpt = here.component "itemEditing" cpt where
  cpt { isEditing, valueBox } _ = do
    valueBox' <- T.useLive T.unequal valueBox

    pure $ H.div { className: "input-group col-sm-6" }
             [ H.input
               { className: "form-control", type: "text"
               , defaultValue: valueBox', disabled: true }
             , H.div { className: "btn input-group-append", on: { click } }
               [ H.div { className: "input-group-text fa fa-pencil" } [] ]
             ]
      where
        click _ = T.write_ true isEditing

itemEditing :: R2.Leaf ItemProps
itemEditing = R2.leafComponent itemEditingCpt
itemEditingCpt :: R.Component ItemProps
itemEditingCpt = here.component "itemNotEditing" cpt where
  cpt { defaultVal, isEditing, lens, onUpdateUserInfo, userInfo, valueBox } _ = do
    valueBox' <- T.useLive T.unequal valueBox

    pure $ H.div { className: "input-group col-sm-6" }
             [ inputWithEnter
               { autoFocus: true
               , className: "form-control"
               , defaultValue: valueBox'
               , onBlur: \v -> T.write_ v valueBox
               , onEnter: click
               , onValueChanged: \v -> do
                   here.log2 "[itemEditingCpt] value Changed: " v
                   T.write_ v valueBox
               , placeholder: defaultVal
               , type: "text" }
             , H.div { className: "btn input-group-append", on: { click } }
               [ H.div { className: "input-group-text fa fa-floppy-o" } [] ]
             ]
      where
        cLens = L.cloneLens lens
        click _ = do
          T.write_ false isEditing
          value <- T.read valueBox
          here.log2 "[itemEditing] value" value
          let newUserInfo = (L.set cLens value userInfo) :: UserInfo
          onUpdateUserInfo newUserInfo


-- saveContactHyperdata :: Session -> Int -> HyperdataContact -> AffRESTError Int
-- saveContactHyperdata session id = put session (Routes.NodeAPI Node (Just id) "")

saveUserInfo :: Session -> Int -> UserInfo ->  AffRESTError Int
saveUserInfo session id ui = do
  let token = getToken session
  client <- liftEffect $ getClient session
  res <- mutation
    client
    "update user_info"
    { update_user_info: onlyArgs { token: token
                                 , ui_id: id
                                 , ui_cwFirstName: ga ui.ui_cwFirstName
                                 , ui_cwLastName: ga ui.ui_cwLastName
                                 , ui_cwOrganization: ui.ui_cwOrganization
                                 , ui_cwLabTeamDepts: ui.ui_cwLabTeamDepts
                                 , ui_cwOffice: ga ui.ui_cwOffice
                                 , ui_cwCity: ga ui.ui_cwCity
                                 , ui_cwCountry: ga ui.ui_cwCountry
                                 , ui_cwRole: ga ui.ui_cwRole
                                 , ui_cwTouchPhone: ga ui.ui_cwTouchPhone
                                 , ui_cwTouchMail: ga ui.ui_cwTouchMail } }
  pure $ Right res.update_user_info
  where
    ga Nothing = ArgL IgnoreArg
    ga (Just val) = ArgR val
    getToken (Session { token }) = token

getUserInfoWithReload :: { nodeId :: Int
                         , reload :: T2.Reload
                         , session :: Session
                         } -> AffRESTError UserInfo
getUserInfoWithReload {nodeId, session} = getUserInfo session nodeId -- getContact session nodeId
