module Gargantext.Components.Nodes.Annuaire.User.Contacts
  ( module Gargantext.Components.Nodes.Annuaire.User.Contacts.Types
  , annuaireUserLayout
  , userLayout )
  where

import Data.Array (head)
import Data.Lens as L
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (Tuple3, (/\))
import Data.Newtype (unwrap)
import Data.String (joinWith)
import DOM.Simple.Console (log2)
import Effect (Effect)
import Effect.Aff (Aff)
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Prelude
import Gargantext.Components.Nodes.Annuaire.User.Contacts.Types
import Gargantext.Components.Nodes.Annuaire.User.Contacts.Tabs as Tabs
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Routes as Routes
import Gargantext.Ends (Frontends)
import Gargantext.Sessions (Session, get)
import Gargantext.Types (NodeType(..))
import Gargantext.Utils.Reactix as R2

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

getFirstName :: Maybe ContactWho -> String
getFirstName obj = fromMaybe "Empty title" $ getFirstName' <$> obj
getFirstName' = fromMaybe "Empty first name" <<< _.firstName <<< unwrap

getLastName obj = fromMaybe "Empty title" $ getLastName' <$> obj
getLastName' = fromMaybe "Empty last name"  <<< _.lastName <<< unwrap

-- | ContactWhere infos
-- TODO factor below
getRole :: Array ContactWhere -> String
getRole = maybe "Empty Contact-Where" getRole' <<< head
  where
    getRole' = fromMaybe "Empty Role" <<< _.role <<< unwrap

getOrga :: Array ContactWhere -> String
getOrga = maybe "Emtpy Contact-Where" getOrga' <<< head
  where
    getOrga' :: ContactWhere -> String
    getOrga' obj = joinWith ", " $ (\(ContactWhere {organization: o}) ->o) obj

getDept :: Array ContactWhere -> String
getDept = maybe "Empty Department" getDept' <<< head
  where
    getDept' :: ContactWhere -> String
    getDept' obj = joinWith ", " $ (\(ContactWhere {labTeamDepts: l}) ->l) obj

getOffice :: Array ContactWhere -> String
getOffice = fromMaybe "Empty Office"
          <<< maybe Nothing (\(ContactWhere {office:x}) -> x)
          <<< head

getCity :: Array ContactWhere -> String
getCity = fromMaybe "Empty City"
          <<< maybe Nothing (\(ContactWhere {city:x}) -> x)
          <<< head

getCountry :: Array ContactWhere -> String
getCountry = fromMaybe "Empty Country"
          <<< maybe Nothing (\(ContactWhere {country:x}) -> x)
          <<< head

-- | ContactWhere / Touch infos
getTouch :: Array ContactWhere -> Maybe ContactTouch
getTouch = maybe Nothing (\(ContactWhere {touch}) -> touch) <<< head

getPhone :: Array ContactWhere -> String
getPhone obj = fromMaybe "Empty touch info" $ getPhone' <$> (getTouch obj)
getPhone' :: ContactTouch -> String
getPhone' = fromMaybe "Empty phone" <<< _.phone <<< unwrap

getMail :: Array ContactWhere -> String
getMail obj = fromMaybe "Empty info" $ getMail' <$> (getTouch obj)
getMail' :: ContactTouch -> String
getMail' = fromMaybe "Empty mail" <<< _.mail <<< unwrap

-- | TODO format data in better design (UI) shape
contactInfos :: HyperdataUser -> (HyperdataUser -> Effect Unit) -> Array R.Element
contactInfos h onUpdateHyperdata = item <$> contactInfoItems
  where
    item (label /\ defaultVal /\ lens) =
      contactInfoItem { hyperdata: h
                      , label
                      , lens
                      , onUpdateHyperdata }

    -- item (name /\ value) =
    --   H.li { className: "list-group-item" }
    --     (infoRender (name /\ (" " <> value)))

contactInfoItems :: Array (Tuple3 String String HyperdataUserLens)
contactInfoItems =
  [ "Last Name"     /\ "Empty Last Name"     /\ (_shared <<< _who <<< _lastName)
  , "First Name"    /\ "Empty First Name"    /\ (_shared <<< _who <<< _firstName)
  , "Organisation"  /\ "Empty Organisation"  /\ (_shared <<< _who <<< _lastName)
  , "Lab/Team/Dept" /\ "Empty Lab/Team/Dept" /\ (_shared <<< _who <<< _lastName)
  , "Office"        /\ "Empty Office"        /\ (_shared <<< _who <<< _lastName)
  , "City"          /\ "Empty City"          /\ (_shared <<< _who <<< _lastName)
  , "Country"       /\ "Empty Country"       /\ (_shared <<< _who <<< _lastName)
  , "Role"          /\ "Empty Role"          /\ (_shared <<< _who <<< _lastName)
  , "Phone"         /\ "Empty Phone"         /\ (_shared <<< _who <<< _lastName)
  , "Mail"          /\ "Empty Mail"          /\ (_shared <<< _who <<< _lastName) ]
contactInfoItems' :: Maybe HyperdataContact -> Array (Tuple String String)
contactInfoItems' Nothing =
  [ "Last Name"     /\ "Empty Last Name"
  , "First Name"    /\ "Empty First Name"
  , "Organisation"  /\ "Empty Organisation"
  , "Lab/Team/Dept" /\ "Empty Lab/Team/Dept"
  , "Office"        /\ "Empty Office"
  , "City"          /\ "Empty City"
  , "Country"       /\ "Empty Country"
  , "Role"          /\ "Empty Role"
  , "Phone"         /\ "Empty Phone"
  , "Mail"          /\ "Empty Mail" ]
contactInfoItems' (Just (HyperdataContact {who:who, ou:ou})) =
  [ "Last Name"     /\ getLastName who
  , "First Name"    /\ getFirstName who
  , "Organisation"  /\ getOrga ou
  , "Lab/Team/Dept" /\ getOrga ou
  , "Office"        /\ getOffice ou
  , "City"          /\ getCity ou
  , "Country"       /\ getCountry ou
  , "Role"          /\ getRole ou
  , "Phone"         /\ getPhone ou
  , "Mail"          /\ getMail ou ]

--type HyperdataUserLens = L.Lens' HyperdataUser String
type HyperdataUserLens = L.ALens' HyperdataUser String

type ContactInfoItemProps =
  (
    hyperdata :: HyperdataUser
  , label :: String
  , lens :: HyperdataUserLens
  , onUpdateHyperdata :: HyperdataUser -> Effect Unit
  )

contactInfoItem :: Record ContactInfoItemProps -> R.Element
contactInfoItem props = R.createElement contactInfoItemCpt props []

contactInfoItemCpt :: R.Component ContactInfoItemProps
--contactInfoItemCpt :: forall r. R.Component ( lens :: L.Lens' HyperdataUser String | r )
contactInfoItemCpt = R.hooksComponent "G.C.N.A.U.C.contactInfoItem" cpt
  where
    cpt {hyperdata, label, lens, onUpdateHyperdata} _ = do
      isEditing <- R.useState' false
      let value = (L.view cLens hyperdata) :: String
      valueRef <- R.useRef value

      pure $ H.li { className: "list-group-item" } [
          H.span { className: "badge badge-default badge-pill"} [ H.text label ]
        , item isEditing valueRef
      ]
      where
        cLens = L.cloneLens lens
        item (false /\ setIsEditing) valueRef =
          H.span {} [
              H.text $ R.readRef valueRef
            , H.span { className: "fa fa-pencil"
                     , on: {click: onClick} } []
          ]
          where
            onClick _ = setIsEditing $ const true
        item (true /\ setIsEditing) valueRef =
          H.span {} [
              H.input { className: "form-control"
                      , defaultValue: R.readRef valueRef
                      , on: {change: \e -> R.setRef valueRef $ R2.unsafeEventValue e} }
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

type LayoutProps = ( frontends :: Frontends, nodeId :: Int, session :: Session )

userLayout :: Record LayoutProps -> R.Element
userLayout props = R.createElement userLayoutCpt props []

userLayoutCpt :: R.Component LayoutProps
userLayoutCpt = R.hooksComponent "G.C.Nodes.Annuaire.User.Contacts.userLayout" cpt
  where
    cpt {frontends, nodeId, session} _ = do
      useLoader nodeId (getContact session) $
        \contactData@{contactNode: Contact {name, hyperdata}} ->
          H.ul { className: "col-md-12 list-group" }
          [ display (fromMaybe "no name" name) (contactInfos hyperdata onUpdateHyperdata)
          , Tabs.tabs {frontends, nodeId, contactData, session} ]

      where
        onUpdateHyperdata :: HyperdataUser -> Effect Unit
        onUpdateHyperdata hd = do
          log2 "[onUpdateHyperdata] hd" hd

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


type AnnuaireLayoutProps =
  ( annuaireId :: Int
  | LayoutProps )


annuaireUserLayout :: Record AnnuaireLayoutProps -> R.Element
annuaireUserLayout props = R.createElement annuaireUserLayoutCpt props []

annuaireUserLayoutCpt :: R.Component AnnuaireLayoutProps
annuaireUserLayoutCpt = R.hooksComponent "G.C.Nodes.Annuaire.User.Contacts.annuaireUserLayout" cpt
  where
    cpt {annuaireId, frontends, nodeId, session} _ = do
      useLoader nodeId (getAnnuaireContact session annuaireId) $
        \contactData@{contactNode: Contact {name, hyperdata}} ->
          H.ul { className: "col-md-12 list-group" }
          [ display (fromMaybe "no name" name) (contactInfos hyperdata onUpdateHyperdata)
          , Tabs.tabs {frontends, nodeId, contactData, session} ]

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
