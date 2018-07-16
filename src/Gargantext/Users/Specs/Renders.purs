module Gargantext.Users.Specs.Renders
       where

import Gargantext.Users.Types

import Data.List (List, toUnfoldable, zip)
import Data.Map (Map, empty, keys, values)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), uncurry)
import Prelude (($), (<<<), (<$>))
import React (ReactElement)
import React.DOM (div, h3, h1, li, span, text, ul, img)
import React.DOM.Props (_id, className, src)
import Thermite (Render)


render :: forall props. Render State props Action
render dispatch _ state _ =
  [
          div [className "col-md-12"]
          $ case state.user of
            (Just (User user)) -> display user.name [userInfos user.hyperdata]
            Nothing -> display "User not found" []
  ]

display :: forall props. String -> Array ReactElement -> Array ReactElement
display title elems =
  [ div [className "container-fluid"]
    [ div [className "row", _id "user-page-header"]
          [ div [className "col-md-6"] [ h3 [] [text title] ]
          , div [className "col-md-8"] []
          , div [className "col-md-2"] [ span [] [text ""] ]
          ]
    , div [className "row", _id "user-page-info"]
          [ div [className "col-md-12"]
            [ div [className "row"]
              [ div [className "col-md-2"]
                    [ img [src "/images/Gargantextuel-212x300.jpg"] [] ]
              , div [className "col-md-1"] []
              , div [className "col-md-8"] elems
              ]
            ]
          ]
     ]
  ]

mapMyMap :: forall k v x. (k -> v -> x) -> Map k v -> Array x
mapMyMap f m = toUnfoldable $ uncurry f <$> zip (keys m) (values m)

infixl 4 mapMyMap as <.~$>

userInfos :: Maybe HyperData -> ReactElement
userInfos hyperdata =
    ul [className "list-group"] $
    listInfo <.~$> (checkMaybe hyperdata)
  where
    checkMaybe (Nothing) = empty
    checkMaybe (Just (HyperData a)) = a

listInfo :: String -> String -> ReactElement
listInfo s ss = listElement $ infoRender s ss

listElement :: forall props. Array ReactElement -> ReactElement
listElement = li [className "list-group-item justify-content-between"]

infoRender :: forall props. String -> String -> Array ReactElement
infoRender title content =
      [ span [] [text title]
      , span [className "badge badge-default badge-pill"] [text content]
      ]
