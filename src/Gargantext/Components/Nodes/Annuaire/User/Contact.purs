module Gargantext.Components.Nodes.Annuaire.User.Contact
  ( module Gargantext.Components.Nodes.Annuaire.User.Contacts.Types
  , contactLayout
  ) where

import Gargantext.Prelude
  ( Unit, bind, const, discard, pure, show, ($), (<$>), (*>), (<<<), (<>) )
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
import Gargantext.Components.Nodes.Annuaire.User.Contacts.Tabs as Tabs
import Gargantext.Components.Nodes.Annuaire.User.Contacts.Types
  ( Contact'(..), ContactData', ContactTouch(..), ContactWhere(..)
  , ContactWho(..), HyperdataContact(..), HyperdataUser(..)
  , _city, _country, _firstName, _labTeamDeptsJoinComma, _lastName
  , _mail, _office, _organizationJoinComma, _ouFirst, _phone, _role
  , _shared, _touch, _who, defaultContactTouch, defaultContactWhere
  , defaultContactWho, defaultHyperdataContact, defaultHyperdataUser )
import Gargantext.Components.Nodes.Lists.Types as LT
import Gargantext.Components.Nodes.Texts.Types as TT
import Gargantext.Ends (Frontends)
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes as Routes
import Gargantext.Sessions (Session, get, put, sessionId)
import Gargantext.Types (NodeType(..), SidePanelState)
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Toestand as T2

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
              ]]]]

-- | TODO format data in better design (UI) shape
contactInfos :: HyperdataContact -> (HyperdataContact -> Effect Unit) -> Array R.Element
contactInfos h onUpdateHyperdata = item <$> contactInfoItems where
  item { label, lens, defaultVal: placeholder } =
    contactInfoItem { label, lens, onUpdateHyperdata, placeholder, hyperdata: h }

contactInfoItems :: Array {label:: String, defaultVal:: String, lens:: HyperdataContactLens}
contactInfoItems =
  [ {label: "Last Name"    , defaultVal: "Empty Last Name"    , lens: _who     <<< _lastName             }
  , {label: "First Name"   , defaultVal: "Empty First Name"   , lens: _who     <<< _firstName            }
  , {label: "Organisation" , defaultVal: "Empty Organisation" , lens: _ouFirst <<< _organizationJoinComma}
  , {label: "Lab/Team/Dept", defaultVal: "Empty Lab/Team/Dept", lens: _ouFirst <<< _labTeamDeptsJoinComma}
  , {label: "Office"       , defaultVal: "Empty Office"       , lens: _ouFirst <<< _office               }
  , {label: "City"         , defaultVal: "Empty City"         , lens: _ouFirst <<< _city                 }
  , {label: "Country"      , defaultVal: "Empty Country"      , lens: _ouFirst <<< _country              }
  , {label: "Role"         , defaultVal: "Empty Role"         , lens: _ouFirst <<< _role                 }
  , {label: "Phone"        , defaultVal: "Empty Phone"        , lens: _ouFirst <<< _touch <<< _phone     }
  , {label: "Mail"         , defaultVal: "Empty Mail"         , lens: _ouFirst <<< _touch <<< _mail      }
  ]

type HyperdataContactLens = L.ALens' HyperdataContact String

type ContactInfoItemProps =
  ( hyperdata         :: HyperdataContact
  , label             :: String
  , lens              :: HyperdataContactLens
  , onUpdateHyperdata :: HyperdataContact -> Effect Unit
  , placeholder       :: String
  )

contactInfoItem :: Record ContactInfoItemProps -> R.Element
contactInfoItem props = R.createElement contactInfoItemCpt props []

contactInfoItemCpt :: R.Component ContactInfoItemProps
contactInfoItemCpt = here.component "contactInfoItem" cpt
  where
    cpt {hyperdata, label, lens, onUpdateHyperdata, placeholder} _ = do
      isEditing <- R.useState' false
      let value = (L.view cLens hyperdata) :: String
      valueRef <- R.useRef value
      pure $
        H.div { className: "form-group row" }
        [ H.span { className: "col-sm-2 col-form-label" } [ H.text label ]
        , item isEditing valueRef ]
      where
        cLens = L.cloneLens lens
        item (false /\ setIsEditing) valueRef =
          H.div { className: "input-group col-sm-6" }
          [ H.input
            { className: "form-control", type: "text"
            , defaultValue: placeholder', disabled: true }
          , H.div { className: "btn input-group-append", on: { click } }
            [ H.div { className: "input-group-text fa fa-pencil" } [] ]]
          where
            placeholder' = R.readRef valueRef
            click _ = setIsEditing $ const true
        item (true /\ setIsEditing) valueRef =
          H.div { className: "input-group col-sm-6" }
          [ inputWithEnter
            { autoFocus: true
            , className: "form-control"
            , defaultValue: R.readRef valueRef
            , onBlur: R.setRef valueRef
            , onEnter: click
            , onValueChanged: R.setRef valueRef
            , placeholder
            , type: "text" }
          , H.div { className: "btn input-group-append", on: { click } }
            [ H.div { className: "input-group-text fa fa-floppy-o" } [] ]]
          where
            click _ = do
              setIsEditing $ const false
              let newHyperdata = (L.over cLens (\_ -> R.readRef valueRef) hyperdata) :: HyperdataContact
              onUpdateHyperdata newHyperdata

listElement :: Array R.Element -> R.Element
listElement = H.li { className: "list-group-item justify-content-between" }

type BasicProps =
  ( frontends      :: Frontends
  , nodeId         :: Int
  , sidePanelState :: T.Box SidePanelState
  , sidePanel      :: T.Box (Maybe (Record TT.SidePanel))
  , tasks          :: T.Box GAT.Storage
  )

type ReloadProps =
  ( reloadForest :: T.Box T2.Reload
  , reloadRoot   :: T.Box T2.Reload
  | BasicProps
  )

type LayoutProps = ( session :: Session | ReloadProps )

type KeyLayoutProps = ( key :: String, session :: Session | ReloadProps )

saveContactHyperdata :: Session -> Int -> HyperdataContact -> Aff Int
saveContactHyperdata session id = put session (Routes.NodeAPI Node (Just id) "")

type AnnuaireLayoutProps = ( annuaireId :: Int, session :: R.Context Session | ReloadProps )

type AnnuaireKeyLayoutProps = ( annuaireId :: Int | KeyLayoutProps )

contactLayout :: R2.Component AnnuaireLayoutProps
contactLayout = R.createElement contactLayoutCpt

contactLayoutCpt :: R.Component AnnuaireLayoutProps
contactLayoutCpt = here.component "contactLayout" cpt where
  cpt { annuaireId
      , frontends
      , nodeId
      , reloadForest
      , reloadRoot
      , session
      , sidePanel
      , sidePanelState
      , tasks } _ = do
    s <- R.useContext session
    let key = show (sessionId s) <> "-" <> show nodeId
    pure $
      contactLayoutWithKey
      { annuaireId
      , frontends
      , key
      , nodeId
      , reloadForest
      , reloadRoot
      , session: s
      , sidePanel
      , sidePanelState
      , tasks
      }

contactLayoutWithKey :: R2.Leaf AnnuaireKeyLayoutProps
contactLayoutWithKey props = R.createElement contactLayoutWithKeyCpt props []

contactLayoutWithKeyCpt :: R.Component AnnuaireKeyLayoutProps
contactLayoutWithKeyCpt = here.component "contactLayoutWithKey" cpt where
    cpt { annuaireId
        , frontends
        , reloadForest
        , reloadRoot
        , nodeId
        , session
        , sidePanel
        , sidePanelState
        , tasks } _ = do
      reload <- T.useBox T2.newReload
      _ <- T.useLive T.unequal reload
      cacheState <- T.useBox LT.CacheOn
      useLoader nodeId (getAnnuaireContact session annuaireId) $
        \contactData@{contactNode: Contact' {name, hyperdata}} ->
          H.ul { className: "col-md-12 list-group" }
               [ display { title: fromMaybe "no name" name }
                         (contactInfos hyperdata (onUpdateHyperdata reload))
               , Tabs.tabs
                   { cacheState
                   , contactData
                   , frontends
                   , nodeId
                   , session
                   , sidePanel
                   , sidePanelState
                   , reloadForest
                   , reloadRoot
                   , tasks } ]
      where
        onUpdateHyperdata :: T.Box T2.Reload -> HyperdataContact -> Effect Unit
        onUpdateHyperdata reload hd =
          launchAff_ $
            saveContactHyperdata session nodeId hd *> liftEffect (T2.reload reload)

getAnnuaireContact :: Session -> Int -> Int -> Aff ContactData'
getAnnuaireContact session annuaireId id = do
  contactNode :: Contact' <- get session $ Routes.NodeAPI Annuaire (Just annuaireId) $ show id
  -- TODO: we need a default list for the pairings
  --defaultListIds <- get $ toUrl endConfigStateful Back (Children NodeList 0 1 Nothing) $ Just id
  --case (head defaultListIds :: Maybe (NodePoly HyperdataList)) of
  --  Just (NodePoly { id: defaultListId }) ->
  --    pure {contactNode, defaultListId}
  --  Nothing ->
  --    throwError $ error "Missing default list"
  pure {contactNode, defaultListId: 424242}
