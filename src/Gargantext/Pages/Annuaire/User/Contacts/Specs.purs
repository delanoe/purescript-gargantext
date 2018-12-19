module Gargantext.Pages.Annuaire.User.Contacts.Specs
       (layoutUser)
       where


import Data.List (List, zipWith, catMaybes, toUnfoldable)
import Data.Map (Map, empty, keys, values, lookup)
import Data.Array (head)
import Data.Semigroup ((<>))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (toUnfoldable) as S
import Data.Tuple (Tuple(..), uncurry)
import Data.Unfoldable (class Unfoldable)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String (joinWith)
import Effect.Aff (Aff)
import Thermite (Render, Spec, defaultPerformAction, simpleSpec, createClass)
import React as React
import React (ReactClass, ReactElement)
import React.DOM (div, h3, img, li, span, text, ul, text)
import React.DOM.Props (_id, className, src)
import Gargantext.Prelude
import Gargantext.Config (toUrl, End(..), NodeType(..))
import Gargantext.Config.REST (get)
import Gargantext.Components.Loader as Loader
import Gargantext.Pages.Annuaire.User.Contacts.Types
import Gargantext.Pages.Annuaire.User.Contacts.Tabs.Specs as Tabs

--type Props = Loader.InnerProps Int Contact

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
contactInfos :: HyperdataContact -> Array ReactElement
contactInfos (HyperdataContact {who:who, ou:ou}) =
  [ li [className "list-group-item"] (infoRender (Tuple "Last Name"    $ " " <> getLastName  who))
  , li [className "list-group-item"] (infoRender (Tuple "First name"   $ " " <> getFirstName who))
  , li [className "list-group-item"] (infoRender (Tuple "Organization" $ " " <> getOrga      ou ))
  , li [className "list-group-item"] (infoRender (Tuple "Lab/Team/Dept"$ " " <> getOrga      ou ))
  , li [className "list-group-item"] (infoRender (Tuple "Office"       $ " " <> getOffice    ou ))
  , li [className "list-group-item"] (infoRender (Tuple "City"         $ " " <> getCity      ou ))
  , li [className "list-group-item"] (infoRender (Tuple "Country"      $ " " <> getCountry   ou ))
  , li [className "list-group-item"] (infoRender (Tuple "Role"         $ " " <> getRole      ou ))
  , li [className "list-group-item"] (infoRender (Tuple "Phone"        $ " " <> getPhone     ou ))
  , li [className "list-group-item"] (infoRender (Tuple "Mail"         $ " " <> getMail      ou ))
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

-- | Below an example of a loader, use all code below and adapt it
-- to your code
-- layoutUser is exported by the module
-- only one subnode: contactLoader which as 2 parameters
--   - path (nodeId)
--   - components (which has to be drawn when loaded
layoutUser :: Spec {} {nodeId :: Int} Void
layoutUser = simpleSpec defaultPerformAction render
  where
    render :: Render {} {nodeId :: Int} Void
    render _ {nodeId} _ _ =
      [ contactLoader { path: nodeId
                      , component: createClass "LayoutUser" layoutUser' (const {})
                      } ]

-- | Take the spec and transform it in React Class
-- put here how to draw the Composant
-- props loaded: what has been loaded by the component loader
layoutUser' :: Spec {} Props Void
layoutUser' = simpleSpec defaultPerformAction render
           <> Tabs.pureTabs
  where
    render :: Render {} Props Void
    render dispatch {loaded: Contact {name, hyperdata}} _ _ =
      [ ul [className "col-md-12 list-group"] $
          display (fromMaybe "no name" name) (contactInfos hyperdata)
      ]

-- | toUrl to get data
getContact :: Int -> Aff Contact
getContact id = get $ toUrl Back Node $ Just id

-- | Change name for you
contactLoaderClass :: ReactClass (Loader.Props Int Contact)
contactLoaderClass = Loader.createLoaderClass "ContactLoader" getContact

-- | Change type according to what has been loaded
contactLoader :: Loader.Props' Int Contact -> ReactElement
contactLoader props = React.createElement contactLoaderClass props []
