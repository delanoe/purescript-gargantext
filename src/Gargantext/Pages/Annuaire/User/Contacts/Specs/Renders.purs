module Gargantext.Pages.Annuaire.User.Contacts.Specs.Renders
       where

import Gargantext.Pages.Annuaire.User.Contacts.Types

import Data.List (List, zipWith, catMaybes, toUnfoldable)
import Data.Map (Map, empty, keys, values, lookup)
import Data.Maybe (Maybe(..))
import Data.Set (toUnfoldable) as S
import Data.Tuple (Tuple(..), uncurry)
import Data.Unfoldable (class Unfoldable)
import Prelude (Void)
import Prelude (($), (<<<), (<$>), flip, class Ord)
import React (ReactElement)
import React.DOM (div, h3, img, li, span, text, ul)
import React.DOM.Props (_id, className, src)
import Thermite (Render)


render :: Render State {} Action
render dispatch _ state _ =
  [
          div [className "col-md-12"]
          $ case state.contact of
            (Just (Contact contact)) -> display contact.name [contactInfos contact.hyperdata]
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
                    [ img [src "/images/Gargantextuel-212x300.jpg"] ]
              , div [className "col-md-1"] []
              , div [className "col-mdData.Unfoldable-8"] elems
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

contactInfos :: HyperData String String -> ReactElement
contactInfos hyperdata =
    ul [className "list-group"] [] {- $
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
  [ span [] [text title]
  , span [className "badge badge-default badge-pill"] [text content]
  ]
