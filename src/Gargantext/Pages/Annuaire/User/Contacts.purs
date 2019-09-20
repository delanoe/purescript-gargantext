module Gargantext.Pages.Annuaire.User.Contacts
  ( module Gargantext.Pages.Annuaire.User.Contacts.Types
  , userLayout )
  where

import Prelude ((<$>))
import Data.List (List, zipWith, catMaybes, toUnfoldable)
import Data.Map (Map, empty, keys, values, lookup)
import Data.Array (head)
import Data.Semigroup ((<>))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (toUnfoldable) as S
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (class Unfoldable)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Effect.Aff (Aff, throwError)
import Effect.Exception (error)
import Reactix as R
import Reactix.DOM.HTML as H
import Gargantext.Prelude
import Gargantext.Config (Ends, BackendRoute(..), NodeType(..), url)
import Gargantext.Config.REST (get)
import Gargantext.Components.Node (NodePoly(..), HyperdataList(..))
import Gargantext.Hooks.Loader (useLoader)
import Gargantext.Pages.Annuaire.User.Contacts.Types
import Gargantext.Pages.Annuaire.User.Contacts.Tabs.Specs as Tabs
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
getTouch = maybe Nothing (\(ContactWhere {touch:x}) -> x) <<< head

getPhone :: Array ContactWhere -> String
getPhone obj = fromMaybe "Empty touch info" $ getPhone' <$> (getTouch obj)
getPhone' :: ContactTouch -> String
getPhone' = fromMaybe "Empty phone" <<< _.phone <<< unwrap

getMail :: Array ContactWhere -> String
getMail obj = fromMaybe "Empty info" $ getMail' <$> (getTouch obj)
getMail' :: ContactTouch -> String
getMail' = fromMaybe "Empty mail" <<< _.mail <<< unwrap

-- | TODO format data in better design (UI) shape
contactInfos :: HyperdataContact -> Array R.Element
contactInfos (HyperdataContact {who:who, ou:ou}) = item <$> items
  where
    items =
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
    item (name /\ value) =
      H.li { className: "list-group-item" }
        (infoRender (name /\ (" " <> value)))

listInfo :: Tuple String String -> R.Element
listInfo s = listElement $ infoRender s

listElement :: Array R.Element -> R.Element
listElement = H.li { className: "list-group-item justify-content-between" }

infoRender :: Tuple String String -> Array R.Element
infoRender (Tuple title content) =
  [ H.span { className: "badge badge-default badge-pill"} [ H.text title ]
  , H.span {} [H.text content] ]

type LayoutProps = ( nodeId :: Int, ends :: Ends )

userLayout :: Record LayoutProps -> R.Element
userLayout props = R.createElement userLayoutCpt props []

userLayoutCpt :: R.Component LayoutProps
userLayoutCpt = R.hooksComponent "G.P.Annuaire.UserLayout" cpt
  where
    cpt {nodeId, ends} _ =
      useLoader nodeId (getContact ends) $
        \contactData@{contactNode: Contact {name, hyperdata}} ->
          H.ul { className: "col-md-12 list-group" }
          [ display (fromMaybe "no name" name) (contactInfos hyperdata)
          , Tabs.tabs {nodeId, contactData, ends} ]

-- | toUrl to get data
getContact :: Ends -> Int -> Aff ContactData
getContact ends id = do
  contactNode <- get $ url ends (NodeAPI NodeContact (Just id))
  -- TODO: we need a default list for the pairings
  --defaultListIds <- get $ toUrl endConfigStateful Back (Children NodeList 0 1 Nothing) $ Just id
  --case (head defaultListIds :: Maybe (NodePoly HyperdataList)) of
  --  Just (NodePoly { id: defaultListId }) ->
  --    pure {contactNode, defaultListId}
  --  Nothing ->
  --    throwError $ error "Missing default list"
  pure {contactNode, defaultListId: 424242}

