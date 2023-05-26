module Gargantext.Components.Annotation.Menu
  ( annotationMenu
  , AnnotationMenu
  ) where

import Gargantext.Prelude

import Data.Maybe (Maybe(..))
import Data.String (toLower)
import Effect (Effect)
import Gargantext.Components.Annotation.Types (MenuType(..), termClass)
import Gargantext.Components.Bootstrap as B
import Gargantext.Components.Bootstrap.Types (ComponentStatus(..))
import Gargantext.Types (TermList(..), termListName)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Annotation.Menu"

type Props =
  ( menuRef :: R.Ref (Maybe (Record AnnotationMenu))
  )

type AnnotationMenu =
  ( closeCallback :: Unit -> Effect Unit
  , redrawMenu    :: T.Box Boolean
  , x             :: Number
  , y             :: Number
  , list          :: Maybe TermList
  , menuType      :: MenuType
  , setList       :: TermList -> Effect Unit -- not a state hook setter
  )

annotationMenu :: R2.Leaf Props
annotationMenu = R2.leaf annotationMenuCpt
annotationMenuCpt :: R.Component Props
annotationMenuCpt = here.component "annotationMenu" cpt where
  cpt { menuRef } _ = do
    -- Render
    pure $

      R2.fromMaybe (R.readRef menuRef) \props' ->

        B.contextMenu
        { x: props'.x
        , y: props'.y
        , closeCallback: props'.closeCallback
        } $
        (addToList props') <$> [ MapTerm, CandidateTerm, StopTerm ]

--------------------------------------------------------------------------

-- addToList :: Record AnnotationMenu -> TermList -> Maybe R.Element
-- addToList {list: Just t'} t
--   | t == t'   = Nothing

-- addToList {menuType, setList} t = Just $
--   B.contextMenuItem
--   { callback: click }
--   [
--     B.icon
--     { name: "circle"
--     , className: "mr-2 " <> termClass t
--     }
--   ,
--     H.text (label menuType)
--   ]

--   where
--     label NewNgram        = "Add to "    <> (toLower $ termListName t)
--     label SetTermListItem = "Change to " <> (toLower $ termListName t)
--     click _ = setList t

addToList :: Record AnnotationMenu -> TermList -> R.Element
addToList {list: Just t', menuType} t
  | t == t' =
    B.contextMenuItem
    { callback: const R.nothing
    , status: Disabled
    }
    [
      B.icon
      { name: "circle"
      , className: "mr-2 disabled-term"
      }
    ,
      H.text (label t menuType)
    ]

addToList {menuType, setList} t =
    B.contextMenuItem
    { callback: const $ setList t }
    [
      B.icon
      { name: "circle"
      , className: "mr-2 " <> termClass t
      }
    ,
      H.text (label t menuType)
    ]

label :: TermList -> MenuType -> String
label t NewNgram        = "Add to "    <> (toLower $ termListName t)
label t SetTermListItem = "Change to " <> (toLower $ termListName t)
