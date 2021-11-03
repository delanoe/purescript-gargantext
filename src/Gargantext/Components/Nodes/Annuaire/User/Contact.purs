module Gargantext.Components.Nodes.Annuaire.User.Contact
  ( module Gargantext.Components.Nodes.Annuaire.User.Contacts.Types
  , contactInfos
  , contactLayout
  , getUserInfo
  , getUserInfoWithReload
  , saveContactHyperdata
  , saveUserInfo
  ) where

import Gargantext.Components.GraphQL.User
import Gargantext.Prelude

import Affjax.RequestBody (RequestBody(..))
import Data.Array as A
import Data.Either (Either(..))
import Data.Lens as L
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Gargantext.Components.App.Data (Boxes)
import Gargantext.Components.GraphQL (queryGql)
import Gargantext.Components.InputWithEnter (inputWithEnter)
import Gargantext.Components.Nodes.Annuaire.User.Contacts.Tabs as Tabs
import Gargantext.Components.Nodes.Annuaire.User.Contacts.Types (Contact'(..), ContactData', ContactTouch(..), ContactWhere(..), ContactWho(..), HyperdataContact(..), HyperdataUser(..), _city, _country, _firstName, _labTeamDeptsJoinComma, _lastName, _mail, _office, _organizationJoinComma, _ouFirst, _phone, _role, _shared, _touch, _who, defaultContactTouch, defaultContactWhere, defaultContactWho, defaultHyperdataContact, defaultHyperdataUser)
import Gargantext.Components.Nodes.Lists.Types as LT
import Gargantext.Config.REST (RESTError(..), logRESTError)
import Gargantext.Ends (Frontends)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes as Routes
import Gargantext.Sessions (Session, get, put, sessionId)
import Gargantext.Types (NodeType(..))
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2
import GraphQL.Client.Args (type (==>), (=>>))
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Nodes.Annuaire.User.Contact"

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
--   
-- contactInfos' :: HyperdataContact -> (HyperdataContact -> Effect Unit) -> Array R.Element
-- contactInfos' h onUpdateHyperdata = item <$> contactInfoItems where
--   item { label, lens, defaultVal: placeholder } =
--     contactInfoItem { label, lens, onUpdateHyperdata, placeholder, hyperdata: h }
-- 
-- contactInfoItems' :: Array {label:: String, defaultVal:: String, lens:: HyperdataContactLens}
-- contactInfoItems' =
--   [ {label: "Last Name"    , defaultVal: "Empty Last Name"    , lens: _who     <<< _lastName             }
--   , {label: "First Name"   , defaultVal: "Empty First Name"   , lens: _who     <<< _firstName            }
--   , {label: "Organisation" , defaultVal: "Empty Organisation" , lens: _ouFirst <<< _organizationJoinComma}
--   , {label: "Lab/Team/Dept", defaultVal: "Empty Lab/Team/Dept", lens: _ouFirst <<< _labTeamDeptsJoinComma}
--   , {label: "Office"       , defaultVal: "Empty Office"       , lens: _ouFirst <<< _office               }
--   , {label: "City"         , defaultVal: "Empty City"         , lens: _ouFirst <<< _city                 }
--   , {label: "Country"      , defaultVal: "Empty Country"      , lens: _ouFirst <<< _country              }
--   , {label: "Role"         , defaultVal: "Empty Role"         , lens: _ouFirst <<< _role                 }
--   , {label: "Phone"        , defaultVal: "Empty Phone"        , lens: _ouFirst <<< _touch <<< _phone     }
--   , {label: "Mail"         , defaultVal: "Empty Mail"         , lens: _ouFirst <<< _touch <<< _mail      }
--   ]
-- 
-- type HyperdataContactLens = L.ALens' HyperdataContact String

type ContactInfoItemProps =
  ( defaultVal       :: String
  , label            :: String
  , lens             :: UserInfoLens
  , onUpdateUserInfo :: UserInfo -> Effect Unit
  , userInfo         :: UserInfo
  )

contactInfoItem :: R2.Leaf ContactInfoItemProps
contactInfoItem props = R.createElement contactInfoItemCpt props []
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
itemNotEditing props = R.createElement itemNotEditingCpt props []
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
itemEditing props = R.createElement itemEditingCpt props []
itemEditingCpt :: R.Component ItemProps
itemEditingCpt = here.component "itemNotEditing" cpt where
  cpt { defaultVal, isEditing, lens, onUpdateUserInfo, userInfo, valueBox } _ = do
    valueBox' <- T.useLive T.unequal valueBox
    
    pure $ H.div { className: "input-group col-sm-6" }
             [ inputWithEnter
               { autoFocus: true
               , className: "form-control"
               , defaultValue: valueBox'
               --, defaultValue: R.readRef valueRef
               , onBlur: \v -> T.write_ v valueBox
               --, onBlur: R.setRef valueRef
               , onEnter: click
               , onValueChanged: \v -> do
                   here.log2 "[itemEditingCpt] value Changed: " v
                   T.write_ v valueBox
               --, onValueChanged: R.setRef valueRef
               , placeholder: defaultVal
               , type: "text" }
             , H.div { className: "btn input-group-append", on: { click } }
               [ H.div { className: "input-group-text fa fa-floppy-o" } [] ]
             ]
      where
        cLens = L.cloneLens lens
        click _ = do
          T.write_ false isEditing
          --let newUserInfo = (L.over cLens (\_ -> R.readRef valueRef) userInfo) :: UserInfo
          value <- T.read valueBox
          here.log2 "[itemEditing] value" value
          let newUserInfo = (L.set cLens value userInfo) :: UserInfo
          onUpdateUserInfo newUserInfo

type ReloadProps =
  ( boxes     :: Boxes
  , frontends :: Frontends
  , nodeId    :: Int
  )

type LayoutProps =
  ( session :: Session
  | ReloadProps )

type KeyLayoutProps =
 ( key :: String
 , session :: Session
 | ReloadProps )

saveContactHyperdata :: Session -> Int -> HyperdataContact -> Aff (Either RESTError Int)
saveContactHyperdata session id = put session (Routes.NodeAPI Node (Just id) "")

saveUserInfo :: Session -> Int -> UserInfo -> Aff (Either RESTError Int)
saveUserInfo session id ui = do
  -- TODO GraphQL
  pure $ Left $ CustomError "TODO implement graphql for saveUserInfo"

type AnnuaireLayoutProps = ( annuaireId :: Int, session :: Session | ReloadProps )

type AnnuaireKeyLayoutProps = ( annuaireId :: Int | KeyLayoutProps )

contactLayout :: R2.Component AnnuaireLayoutProps
contactLayout = R.createElement contactLayoutCpt
contactLayoutCpt :: R.Component AnnuaireLayoutProps
contactLayoutCpt = here.component "contactLayout" cpt where
  cpt props@{ nodeId
            , session } _ = do
    let key = show (sessionId session) <> "-" <> show nodeId
    pure $
      contactLayoutWithKey $ Record.merge props { key }

contactLayoutWithKey :: R2.Leaf AnnuaireKeyLayoutProps
contactLayoutWithKey props = R.createElement contactLayoutWithKeyCpt props []
contactLayoutWithKeyCpt :: R.Component AnnuaireKeyLayoutProps
contactLayoutWithKeyCpt = here.component "contactLayoutWithKey" cpt where
    cpt { annuaireId
        , boxes: boxes@{ sidePanelTexts }
        , frontends
        , nodeId
        , session } _ = do
      reload <- T.useBox T2.newReload
      _ <- T.useLive T.unequal reload
      cacheState <- T.useBox LT.CacheOn
      useLoader { errorHandler
                --, loader: getAnnuaireContact session annuaireId
                , loader: getUserInfo session
                , path: nodeId
                , render: \userInfo@{ ui_username } ->
                    H.ul { className: "col-md-12 list-group" }
                      [ display { title: fromMaybe "no name" (Just ui_username) }
                        (contactInfos userInfo (onUpdateUserInfo reload))
                      , Tabs.tabs
                        { boxes
                        , cacheState
                        , defaultListId: 424242 -- TODO
                        , frontends
                        , nodeId
                        , session
                        , sidePanel: sidePanelTexts
                        }
                      ]
                }
      where
        errorHandler = logRESTError here "[contactLayoutWithKey]"
        onUpdateUserInfo :: T2.ReloadS -> UserInfo -> Effect Unit
        onUpdateUserInfo reload ui = do
          launchAff_ $ do
            _ <- saveUserInfo session nodeId ui
            liftEffect (T2.reload reload)

getAnnuaireContact :: Session -> Int -> Int -> Aff (Either RESTError ContactData')
getAnnuaireContact session annuaireId id = do
  eContactNode <- get session $ Routes.NodeAPI Annuaire (Just annuaireId) $ show id
  -- TODO: we need a default list for the pairings
  --defaultListIds <- get $ toUrl endConfigStateful Back (Children NodeList 0 1 Nothing) $ Just id
  --case (head defaultListIds :: Maybe (NodePoly HyperdataList)) of
  --  Just (NodePoly { id: defaultListId }) ->
  --    pure {contactNode, defaultListId}
  --  Nothing ->
  --    throwError $ error "Missing default list"
  pure $ (\contactNode -> { contactNode, defaultListId: 424242 }) <$> eContactNode


getUserInfoWithReload :: { nodeId :: Int
                         , reload :: T2.Reload
                         , session :: Session} -> Aff (Either RESTError UserInfo)
getUserInfoWithReload {nodeId, session} = getUserInfo session nodeId -- getContact session nodeId

getUserInfo :: Session -> Int -> Aff (Either RESTError UserInfo)
getUserInfo session id = do
  { user_infos } <- queryGql "get user infos"
                    { user_infos: { user_id: id } =>>
                      { ui_id
                      , ui_username
                      , ui_email
                      , ui_title
                      , ui_source
                      , ui_cwFirstName
                      , ui_cwLastName
                      , ui_cwCity
                      , ui_cwCountry
                      , ui_cwLabTeamDepts
                      , ui_cwOrganization
                      , ui_cwOffice
                      , ui_cwRole
                      , ui_cwTouchMail
                      , ui_cwTouchPhone }
                    }
  liftEffect $ here.log2 "[getUserInfo] user infos" user_infos
  pure $ case A.head user_infos of
    Nothing -> Left (CustomError $ "user with id " <> show id <> " not found")
    -- NOTE Contact is at G.C.N.A.U.C.Types
    Just ui -> Right ui
--     Just ui -> Right $ { contactNode: Contact
--                            { id: ui.ui_id
--                            , date: Nothing
--                            , hyperdata: HyperdataUser
--                              { shared: Just $ HyperdataContact
--                                { bdd: Nothing
--                                , who: Just $ ContactWho
--                                   { idWho: Nothing
--                                   , firstName: ui.ui_cwFirstName
--                                   , lastName: ui.ui_cwLastName
--                                   , keywords: []
--                                   , freetags: []
--                                   }
--                                , ou:
--                                  [ ContactWhere
--                                    { organization: ui.ui_cwOrganization
--                                    , labTeamDepts: ui.ui_cwLabTeamDepts
--                                    , role: ui.ui_cwRole
--                                    , office: ui.ui_cwOffice
--                                    , country: ui.ui_cwCountry
--                                    , city: ui.ui_cwCity
--                                    , touch: Nothing  -- TODO
--                                    , entry: Nothing
--                                    , exit: Nothing }
--                                  ]
--                                , source: ui.ui_source
--                                , title: ui.ui_title
--                                , lastValidation: Nothing
--                                , uniqId: Nothing
--                                , uniqIdBdd: Nothing
--                                }
--                              }
--                            , name: Just ui.ui_username
--                            , parentId: Nothing
--                            , typename: Nothing
--                            , userId: Just ui.ui_id }
--                        , defaultListId: 424242 }
--
--getUser' :: Session -> Int -> Aff (Either RESTError ContactData)
--getUser' session id = do
--  { users } <- queryGql "get user"
--               { users: { user_id: id } =>>
--                 { u_id
--                 , u_hyperdata:
--                   { shared:
--                     { title
--                     , source
--                     , who:
--                       { firstName
--                       , lastName }
--                     , "where":
--                       { organization }
--                     }
--                   }
--                 , u_username
--                 , u_email } }
--  liftEffect $ here.log2 "[getUser] users" users
--  pure $ case A.head users of
--    Nothing -> Left (CustomError $ "user with id " <> show id <> " not found")
--    -- NOTE Contact is at G.C.N.A.U.C.Types
--    Just u  -> Right $ { contactNode: Contact
--                           { id: u.u_id
--                           , date: Nothing
--                           , hyperdata: HyperdataUser
--                             { shared: (\shared -> HyperdataContact
--                               { bdd: Nothing
--                               , who: (\who -> ContactWho
--                                  { idWho: Nothing
--                                  , firstName: who.firstName
--                                  , lastName: who.lastName
--                                  , keywords: []
--                                  , freetags: []
--                                  }) <$> shared.who
--                               , ou: (\ou -> ContactWhere
--                                   { organization: ou.organization
--                                   , labTeamDepts: []
--                                   , role: Nothing
--                                   , office: Nothing
--                                   , country: Nothing
--                                   , city: Nothing
--                                   , touch: Nothing
--                                   , entry: Nothing
--                                   , exit: Nothing }) <$> shared.where
--                               , source: shared.source
--                               , title: shared.title
--                               , lastValidation: Nothing
--                               , uniqId: Nothing
--                               , uniqIdBdd: Nothing
--                               }) <$> u.u_hyperdata.shared
--                             }
--                           , name: Just u.u_username
--                           , parentId: Nothing
--                           , typename: Nothing
--                           , userId: Just u.u_id }
--                       , defaultListId: 424242 }
