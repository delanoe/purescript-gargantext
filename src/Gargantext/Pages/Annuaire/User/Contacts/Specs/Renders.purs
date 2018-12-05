module Gargantext.Pages.Annuaire.User.Contacts.Specs.Renders
       where

import Gargantext.Pages.Annuaire.User.Contacts.Types

import Prelude (map)
import Data.List (List, zipWith, catMaybes, toUnfoldable)
import Data.Map (Map, empty, keys, values, lookup)
import Data.Array (head)
import Data.Semigroup ((<>))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (toUnfoldable) as S
import Data.Tuple (Tuple(..), uncurry)
import Data.Unfoldable (class Unfoldable)
import Prelude (identity)
import Prelude (($), (<<<), (<$>), flip, class Ord)
import React (ReactElement)
import React.DOM (div, h3, img, li, span, text, ul, text)
import React.DOM.Props (_id, className, src)
import Thermite (Render)
import Data.Newtype (unwrap)
import Data.String

render :: Render State {} Action
render dispatch _ state _ =
  [
          div [className "col-md-12"]
          $ case state.contact of
            (Just (Contact contact)) -> display (fromMaybe "no name" contact.name) (contactInfos contact.hyperdata)
            Nothing -> display "Contact not found" []
  ]

display :: String -> Array ReactElement -> Array ReactElement
display title elems =
  [ div [className "container-fluid"]
    [ div [className "row", _id "contact-page-header"]
          [ div [className "col-md-6"] [ h3 [] [text title] ]
          , div [className "col-md-8"] []
          , div [className "col-md-2"] [ span [] [text ""] ]
          ]
    , div [className "row", _id "contact-page-info"]
          [ div [className "col-md-12"]
            [ div [className "row"]
              [ div [className "col-md-2"]
                    --[ ]
                    [ img [src "/images/Gargantextuel-212x300.jpg"] ]
              , div [className "col-md-1"] []
              , div [className "col-md-8"] elems
              ]
            ]
          ]
     ]
  ]

mapMyMap :: forall k v x f. Ord k => Unfoldable f => (k -> v -> x) -> Map k v -> f x
mapMyMap f m = toUnfoldable
               $ zipWith f mapKeys
               (catMaybes $ flip lookup m <$> mapKeys)
  where mapKeys = S.toUnfoldable $ keys m

infixl 4 mapMyMap as <.~$>

getFirstName obj = fromMaybe "Empty title" $ getFirstName' <$> obj
getFirstName' = fromMaybe "Empty first name" <<< _.firstName <<< unwrap

getLastName obj = fromMaybe "Empty title" $ getLastName' <$> obj
getLastName' = fromMaybe "Empty last name"  <<< _.lastName <<< unwrap

-- | ContactWhere infos
-- TODO factor below
getRole :: Array ContactWhere -> String
getRole obj = joinWith ", " $ getRole' <$> obj
getRole' = fromMaybe "no role" <<< _.role <<< unwrap

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
getPhone obj = fromMaybe "" $ getPhone' <$> (getTouch obj)
getPhone' :: ContactTouch -> String
getPhone' = fromMaybe "no phone" <<< _.phone <<< unwrap

getMail :: Array ContactWhere -> String
getMail obj = fromMaybe "" $ getMail' <$> (getTouch obj)
getMail' :: ContactTouch -> String
getMail' = fromMaybe "no mail" <<< _.mail <<< unwrap

-- | TODO format data in better design (UI) shape
contactInfos :: HyperdataContact -> Array ReactElement
contactInfos (HyperdataContact {who:who, ou:ou}) =
  [ ul [className "list-group"] (infoRender (Tuple "Last Name"    $ " " <> getLastName  who))
  , ul [className "list-group"] (infoRender (Tuple "First name"   $ " " <> getFirstName who))
  , ul [className "list-group"] (infoRender (Tuple "Organization" $ " " <> getOrga      ou ))
  , ul [className "list-group"] (infoRender (Tuple "Lab/Team/Dept"$ " " <> getOrga      ou ))
  , ul [className "list-group"] (infoRender (Tuple "Office"       $ " " <> getOffice      ou ))
  , ul [className "list-group"] (infoRender (Tuple "City"         $ " " <> getCity      ou ))
  , ul [className "list-group"] (infoRender (Tuple "Country"      $ " " <> getCountry      ou ))
  , ul [className "list-group"] (infoRender (Tuple "Role"         $ " " <> getRole      ou ))
  , ul [className "list-group"] (infoRender (Tuple "Phone"        $ " " <> getPhone     ou ))
  , ul [className "list-group"] (infoRender (Tuple "Mail"         $ " " <> getMail      ou ))
  ]

  
  {- $
    listInfo <.~$> hyperdata
  where
    checkMaybe (Nothing) = empty
    checkMaybe (Just (HyperData a)) = a
-}

listInfo :: Tuple String String -> ReactElement
listInfo s = listElement $ infoRender s

listElement :: Array ReactElement -> ReactElement
listElement = li [className "list-group-item justify-content-between"]

infoRender :: Tuple String String -> Array ReactElement
infoRender (Tuple title content) =
  [ span [className "badge badge-default badge-pill"] [text title]
  , span [] [text content]
  ]
