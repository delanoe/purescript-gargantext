module Gargantext.Components.Nodes.Annuaire.User.Contacts
  ( module Gargantext.Components.Nodes.Annuaire.User.Contacts.Types
  , annuaireUserLayout
  , userLayout )
  where

import DOM.Simple.Console (log2)
import Data.Lens as L
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.AsyncTasks as GAT
import Gargantext.Components.InputWithEnter (inputWithEnter)
import Gargantext.Components.Nodes.Annuaire.User.Contacts.Tabs as Tabs
import Gargantext.Components.Nodes.Annuaire.User.Contacts.Types (Contact(..), ContactData, ContactTouch(..), ContactWhere(..), ContactWho(..), HyperdataContact(..), HyperdataUser(..), _city, _country, _firstName, _labTeamDeptsJoinComma, _lastName, _mail, _office, _organizationJoinComma, _ouFirst, _phone, _role, _shared, _touch, _who, defaultContactTouch, defaultContactWhere, defaultContactWho, defaultHyperdataContact, defaultHyperdataUser)
import Gargantext.Components.Nodes.Lists.Types as NT
import Gargantext.Ends (Frontends)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Prelude (Unit, bind, const, discard, pure, show, unit, ($), (+), (<$>), (<<<), (<>), (==))
import Gargantext.Routes as Routes
import Gargantext.Sessions (Session, get, put, sessionId)
import Gargantext.Types (NodeType(..))

thisModule :: String
thisModule = "Gargantext.Components.Nodes.Annuaire.User.Contacts"

display :: String -> Array R.Element -> R.Element
display title elems =
  H.div { className: "container-fluid" }
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
        , H.div { className: "col-md-8"} elems
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
contactInfoItemCpt = R.hooksComponentWithModule thisModule "contactInfoItem" cpt
  where
    cpt {hyperdata, label, lens, onUpdateHyperdata, placeholder} _ = do
      isEditing <- R.useState' false
      let value = (L.view cLens hyperdata) :: String
      valueRef <- R.useRef value

      pure $ H.li { className: "list-group-item" } [
          H.span { className: "badge badge-default badge-pill"} [ H.text label ]
        , item isEditing valueRef
      ]
      where
        cLens = L.cloneLens lens
        usePlaceholder valueRef =
          if R.readRef valueRef == "" then
            Tuple true placeholder
          else
            Tuple false $ R.readRef valueRef
        item (false /\ setIsEditing) valueRef =
          H.span {} [
              H.span { className: if (fst $ usePlaceholder valueRef) then "text-muted" else "" } [
                H.text $ snd $ usePlaceholder valueRef
              ]
            , H.span { className: "fa fa-pencil"
                     , on: {click: onClick} } []
          ]
          where
            onClick _ = setIsEditing $ const true
        item (true /\ setIsEditing) valueRef =
          H.span {} [
              inputWithEnter {
                  onEnter: onClick
                , onValueChanged: R.setRef valueRef
                , autoFocus: true
                , className: "form-control"
                , defaultValue: R.readRef valueRef
                , placeholder
                , type: "text"
                }
            , H.span { className: "fa fa-floppy-o"
                     , on: {click: onClick} } []
          ]
          where
            onClick _ = do
              setIsEditing $ const false
              let newHyperdata = (L.over cLens (\_ -> R.readRef valueRef) hyperdata) :: HyperdataUser
              onUpdateHyperdata newHyperdata

listInfo :: Tuple String String -> R.Element
listInfo s = listElement $ infoRender s

listElement :: Array R.Element -> R.Element
listElement = H.li { className: "list-group-item justify-content-between" }

infoRender :: Tuple String String -> Array R.Element
infoRender (Tuple title content) =
  [ H.span { className: "badge badge-default badge-pill"} [ H.text title ]
  , H.span {} [H.text content] ]

type LayoutProps = (
    appReload     :: R.State Int
  , asyncTasksRef :: R.Ref (Maybe GAT.Reductor)
  , frontends     :: Frontends
  , nodeId        :: Int
  , session       :: Session
  , treeReloadRef :: R.Ref (Maybe (R.State Int))
  )

type KeyLayoutProps = (
    key :: String
  | LayoutProps
  )

userLayout :: Record LayoutProps -> R.Element
userLayout props = R.createElement userLayoutCpt props []

userLayoutCpt :: R.Component LayoutProps
userLayoutCpt = R.hooksComponentWithModule thisModule "userLayout" cpt
  where
    cpt { appReload, asyncTasksRef, frontends, nodeId, session, treeReloadRef } _ = do
      let sid = sessionId session

      pure $ userLayoutWithKey {
          appReload
        , asyncTasksRef
        , frontends
        , key: show sid <> "-" <> show nodeId
        , nodeId
        , session
        , treeReloadRef
        }

userLayoutWithKey :: Record KeyLayoutProps -> R.Element
userLayoutWithKey props = R.createElement userLayoutWithKeyCpt props []

userLayoutWithKeyCpt :: R.Component KeyLayoutProps
userLayoutWithKeyCpt = R.hooksComponentWithModule thisModule "userLayoutWithKey" cpt
  where
    cpt { appReload, asyncTasksRef, frontends, nodeId, session, treeReloadRef } _ = do
      reload <- R.useState' 0

      cacheState <- R.useState' NT.CacheOn

      useLoader {nodeId, reload: fst reload, session} getContactWithReload $
        \contactData@{contactNode: Contact {name, hyperdata}} ->
          H.ul { className: "col-md-12 list-group" } [
            display (fromMaybe "no name" name) (contactInfos hyperdata (onUpdateHyperdata reload))
          , Tabs.tabs {
                 appReload
               , asyncTasksRef
               , cacheState
               , contactData
               , frontends
               , nodeId
               , session
               , treeReloadRef
               }
          ]
      where
        onUpdateHyperdata :: R.State Int -> HyperdataUser -> Effect Unit
        onUpdateHyperdata (_ /\ setReload) hd = do
          log2 "[onUpdateHyperdata] hd" hd
          launchAff_ $ do
            _ <- saveContactHyperdata session nodeId hd
            liftEffect $ setReload $ (+) 1

-- | toUrl to get data
getContact :: Session -> Int -> Aff ContactData
getContact session id = do
  contactNode <- get session $ Routes.NodeAPI Node (Just id) ""
  -- TODO: we need a default list for the pairings
  --defaultListIds <- get $ toUrl endConfigStateful Back (Children NodeList 0 1 Nothing) $ Just id
  --case (head defaultListIds :: Maybe (NodePoly HyperdataList)) of
  --  Just (NodePoly { id: defaultListId }) ->
  --    pure {contactNode, defaultListId}
  --  Nothing ->
  --    throwError $ error "Missing default list"
  pure {contactNode, defaultListId: 424242}

getContactWithReload :: {nodeId :: Int, reload :: Int, session :: Session} -> Aff ContactData
getContactWithReload {nodeId, session} = getContact session nodeId

saveContactHyperdata :: Session -> Int -> HyperdataUser -> Aff Int
saveContactHyperdata session id h = do
  put session (Routes.NodeAPI Node (Just id) "") h


type AnnuaireLayoutProps = (
    annuaireId :: Int
  | LayoutProps )


annuaireUserLayout :: Record AnnuaireLayoutProps -> R.Element
annuaireUserLayout props = R.createElement annuaireUserLayoutCpt props []

annuaireUserLayoutCpt :: R.Component AnnuaireLayoutProps
annuaireUserLayoutCpt = R.hooksComponentWithModule thisModule "annuaireUserLayout" cpt
  where
    cpt { annuaireId, appReload, asyncTasksRef, frontends, nodeId, session, treeReloadRef } _ = do
      cacheState <- R.useState' NT.CacheOn

      useLoader nodeId (getAnnuaireContact session annuaireId) $
        \contactData@{contactNode: Contact {name, hyperdata}} ->
          H.ul { className: "col-md-12 list-group" } [
            display (fromMaybe "no name" name) (contactInfos hyperdata onUpdateHyperdata)
          , Tabs.tabs {
                 appReload
               , asyncTasksRef
               , cacheState
               , contactData
               , frontends
               , nodeId
               , session
               , treeReloadRef
               }
          ]

      where
        onUpdateHyperdata :: HyperdataUser -> Effect Unit
        onUpdateHyperdata _ = pure unit

getAnnuaireContact :: Session -> Int -> Int -> Aff ContactData
getAnnuaireContact session annuaireId id = do
  contactNode <- get session $ Routes.NodeAPI Annuaire (Just annuaireId) $ "contact/" <> (show id)
  -- TODO: we need a default list for the pairings
  --defaultListIds <- get $ toUrl endConfigStateful Back (Children NodeList 0 1 Nothing) $ Just id
  --case (head defaultListIds :: Maybe (NodePoly HyperdataList)) of
  --  Just (NodePoly { id: defaultListId }) ->
  --    pure {contactNode, defaultListId}
  --  Nothing ->
  --    throwError $ error "Missing default list"
  pure {contactNode, defaultListId: 424242}
