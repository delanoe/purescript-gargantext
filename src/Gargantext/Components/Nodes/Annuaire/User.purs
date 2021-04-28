module Gargantext.Components.Nodes.Annuaire.User
  ( module Gargantext.Components.Nodes.Annuaire.User.Contacts.Types
  , userLayout
  )
  where

import Gargantext.Prelude (Unit, bind, const, discard, pure, show, ($), (<$>), (<<<), (<>))
import Data.Lens as L
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

import Gargantext.AsyncTasks as GAT
import Gargantext.Components.InputWithEnter (inputWithEnter)
import Gargantext.Components.Nodes.Annuaire.User.Contacts.Types (Contact(..), ContactData, ContactTouch(..), ContactWhere(..), ContactWho(..), HyperdataContact(..), HyperdataUser(..), _city, _country, _firstName, _labTeamDeptsJoinComma, _lastName, _mail, _office, _organizationJoinComma, _ouFirst, _phone, _role, _shared, _touch, _who, defaultContactTouch, defaultContactWhere, defaultContactWho, defaultHyperdataContact, defaultHyperdataUser)
import Gargantext.Components.Nodes.Annuaire.Tabs as Tabs
import Gargantext.Components.Nodes.Lists.Types as LT
import Gargantext.Components.Nodes.Texts.Types as TT
import Gargantext.Ends (Frontends)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes as Routes
import Gargantext.Sessions (WithSession, WithSessionContext, Session, get, put, sessionId)
import Gargantext.Types (NodeType(..), SidePanelState)
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2

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
              ]]]]

-- | TODO format data in better design (UI) shape
contactInfos :: HyperdataUser -> (HyperdataUser -> Effect Unit) -> Array R.Element
contactInfos h onUpdateHyperdata = item <$> contactInfoItems
  where
    item {label, defaultVal, lens} =
      contactInfoItem { hyperdata: h
                      , label
                      , lens
                      , onUpdateHyperdata
                      , placeholder: defaultVal }

contactInfoItems :: Array {label:: String, defaultVal:: String, lens:: HyperdataUserLens}
contactInfoItems =
  [ {label: "Last Name"    , defaultVal: "Empty Last Name"    , lens: _shared <<< _who     <<< _lastName             }
  , {label: "First Name"   , defaultVal: "Empty First Name"   , lens: _shared <<< _who     <<< _firstName            }
  , {label: "Organisation" , defaultVal: "Empty Organisation" , lens: _shared <<< _ouFirst <<< _organizationJoinComma}
  , {label: "Lab/Team/Dept", defaultVal: "Empty Lab/Team/Dept", lens: _shared <<< _ouFirst <<< _labTeamDeptsJoinComma}
  , {label: "Office"       , defaultVal: "Empty Office"       , lens: _shared <<< _ouFirst <<< _office               }
  , {label: "City"         , defaultVal: "Empty City"         , lens: _shared <<< _ouFirst <<< _city                 }
  , {label: "Country"      , defaultVal: "Empty Country"      , lens: _shared <<< _ouFirst <<< _country              }
  , {label: "Role"         , defaultVal: "Empty Role"         , lens: _shared <<< _ouFirst <<< _role                 }
  , {label: "Phone"        , defaultVal: "Empty Phone"        , lens: _shared <<< _ouFirst <<< _touch <<< _phone     }
  , {label: "Mail"         , defaultVal: "Empty Mail"         , lens: _shared <<< _ouFirst <<< _touch <<< _mail      }
  ]

type HyperdataUserLens = L.ALens' HyperdataUser String

type ContactInfoItemProps =
  ( hyperdata :: HyperdataUser
  , label :: String
  , lens :: HyperdataUserLens
  , onUpdateHyperdata :: HyperdataUser -> Effect Unit
  , placeholder :: String
  )

contactInfoItem :: Record ContactInfoItemProps -> R.Element
contactInfoItem props = R.createElement contactInfoItemCpt props []

contactInfoItemCpt :: R.Component ContactInfoItemProps
contactInfoItemCpt = here.component "contactInfoItem" cpt
  where
    cpt {hyperdata, label, lens, onUpdateHyperdata, placeholder} _ = do
      isEditing <- T.useBox false
      isEditing' <- T.useLive T.unequal isEditing

      let value = (L.view cLens hyperdata) :: String
      valueRef <- R.useRef value

      pure $ H.div { className: "form-group row" } [
        H.span { className: "col-sm-2 col-form-label" } [ H.text label ]
      , item isEditing' isEditing valueRef
      ]

      where
        cLens = L.cloneLens lens
        item false isEditing valueRef =
          H.div { className: "input-group col-sm-6" } [
            H.input { className: "form-control"
                    , defaultValue: placeholder'
                    , disabled: 1
                    , type: "text" }
          , H.div { className: "btn input-group-append"
                  , on: { click: onClick } } [
              H.div { className: "input-group-text fa fa-pencil" } []
            ]
          ]
          where
            placeholder' = R.readRef valueRef
            onClick _ = T.write_ true isEditing
        item true isEditing valueRef =
          H.div { className: "input-group col-sm-6" } [
            inputWithEnter {
                autoFocus: true
              , className: "form-control"
              , defaultValue: R.readRef valueRef
              , onBlur: R.setRef valueRef
              , onEnter: onClick
              , onValueChanged: R.setRef valueRef
              , placeholder
              , type: "text"
              }
          , H.div { className: "btn input-group-append"
                  , on: { click: onClick } } [
              H.div { className: "input-group-text fa fa-floppy-o" } []
            ]
          ]
          where
            onClick _ = do
              T.write_ true isEditing
              let newHyperdata = (L.over cLens (\_ -> R.readRef valueRef) hyperdata) :: HyperdataUser
              onUpdateHyperdata newHyperdata

listElement :: Array R.Element -> R.Element
listElement = H.li { className: "list-group-item justify-content-between" }

type LayoutNoSessionProps =
  ( frontends      :: Frontends
  , nodeId         :: Int
  , reloadForest   :: T2.ReloadS
  , reloadRoot     :: T2.ReloadS
  , sidePanel      :: T.Box (Maybe (Record TT.SidePanel))
  , sidePanelState :: T.Box SidePanelState
  , tasks          :: T.Box GAT.Storage
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
    cpt { frontends
        , nodeId
        , reloadForest
        , reloadRoot
        , session
        , sidePanel
        , sidePanelState
        , tasks } _ = do
      let sid = sessionId session

      pure $ userLayoutWithKey {
          frontends
        , key: show sid <> "-" <> show nodeId
        , nodeId
        , reloadForest
        , reloadRoot
        , session
        , sidePanel
        , sidePanelState
        , tasks
        }

userLayoutWithKey :: Record KeyLayoutProps -> R.Element
userLayoutWithKey props = R.createElement userLayoutWithKeyCpt props []

userLayoutWithKeyCpt :: R.Component KeyLayoutProps
userLayoutWithKeyCpt = here.component "userLayoutWithKey" cpt
  where
    cpt { frontends
        , nodeId
        , reloadForest
        , reloadRoot
        , session
        , sidePanel
        , sidePanelState
        , tasks } _ = do
      reload <- T.useBox T2.newReload
      reload' <- T.useLive T.unequal reload

      cacheState <- T.useBox LT.CacheOn

      useLoader {nodeId, reload: reload', session} getUserWithReload $
        \contactData@{contactNode: Contact {name, hyperdata}} ->
          H.ul { className: "col-md-12 list-group" } [
            display { title: fromMaybe "no name" name }
                    (contactInfos hyperdata (onUpdateHyperdata reload))
          , Tabs.tabs {
                 cacheState
               , contactData
               , frontends
               , nodeId
               , reloadForest
               , reloadRoot
               , session
               , sidePanel
        , sidePanelState
               , tasks
               }
          ]
      where
        onUpdateHyperdata :: T2.ReloadS -> HyperdataUser -> Effect Unit
        onUpdateHyperdata reload hd = do
          launchAff_ $ do
            _ <- saveContactHyperdata session nodeId hd
            liftEffect $ T2.reload reload

-- | toUrl to get data XXX
getContact :: Session -> Int -> Aff ContactData
getContact session id = do
  contactNode :: Contact <- get session $ Routes.NodeAPI Node (Just id) ""
  -- TODO: we need a default list for the pairings
  --defaultListIds <- get $ toUrl endConfigStateful Back (Children NodeList 0 1 Nothing) $ Just id
  --case (head defaultListIds :: Maybe (NodePoly HyperdataList)) of
  --  Just (NodePoly { id: defaultListId }) ->
  --    pure {contactNode, defaultListId}
  --  Nothing ->
  --    throwError $ error "Missing default list"
  pure {contactNode, defaultListId: 424242}

getUserWithReload :: { nodeId :: Int
                     , reload :: T2.Reload
                     , session :: Session} -> Aff ContactData
getUserWithReload {nodeId, session} = getContact session nodeId

saveContactHyperdata :: Session -> Int -> HyperdataUser -> Aff Int
saveContactHyperdata session id h = do
  put session (Routes.NodeAPI Node (Just id) "") h

