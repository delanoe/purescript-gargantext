module Gargantext.Pages.Annuaire.User.Contacts.Specs.Renders
       where

import Gargantext.Pages.Annuaire.User.Contacts.Types

import Data.List (List, zipWith, catMaybes, toUnfoldable)
import Data.Map (Map, empty, keys, values, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
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
import Data.Array

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

getFirstName obj = fromMaybe "no title" $ getFirstName' <$> obj
getFirstName' = fromMaybe "no first name" <<< _.firstName <<< unwrap

getLastName obj = fromMaybe "no title" $ getLastName' <$> obj
getLastName' = fromMaybe "no last name"  <<< _.lastName <<< unwrap

getRole obj = joinWith ", " $ getRole' <$> (fromMaybe [] obj)
getRole' = fromMaybe "no role" <<< _.role <<< unwrap

getTouch :: Maybe (Array ContactWhere) -> Array ContactTouch
getTouch obj = fn aryMaybeContactTouch
  where
    aryContactWhere = fromMaybe [] obj
    aryMaybeContactTouch = (_.touch <<< unwrap) <$> aryContactWhere


fn :: forall a. Array (Maybe a) -> Array a
fn obj = concat $ fn' <$> obj
  where 
    fn' :: Maybe a -> Array a
    fn' Nothing = []
    fn' (Just v) = [v]

getPhone :: Maybe (Array ContactWhere) -> String
getPhone obj = joinWith ", " $ getPhone' <$> (getTouch obj)

getPhone' :: ContactTouch -> String
getPhone' = fromMaybe "no phone" <<< _.phone <<< unwrap

getMail :: Maybe (Array ContactWhere) -> String
getMail obj = joinWith ", " $ getMail' <$> (getTouch obj)

getMail' :: ContactTouch -> String
getMail' = fromMaybe "no mail" <<< _.mail <<< unwrap

contactInfos :: HyperdataContact -> Array ReactElement
contactInfos (HyperdataContact {who:who, ou:ou}) =
  [ ul [className "list-group"] (infoRender (Tuple "Last Name"  $ getLastName who))
  , ul [className "list-group"] (infoRender (Tuple "First name" $ getFirstName who))
  , ul [className "list-group"] (infoRender (Tuple "Role" $ getRole ou))
  , ul [className "list-group"] (infoRender (Tuple "Phone" $ getPhone ou))
  , ul [className "list-group"] (infoRender (Tuple "Mail" $ getMail ou))
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
