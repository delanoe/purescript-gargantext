-- | A ContextMenU that allows you to add terms to a list
module Gargantext.Components.Annotation.Menu where


import Data.Array as A
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import Reactix as R
import Reactix.DOM.HTML as HTML
import Toestand as T

import Gargantext.Prelude

import Gargantext.Types (TermList(..), termListName)
import Gargantext.Components.Annotation.Utils (termBootstrapClass)

import Gargantext.Components.ContextMenu.ContextMenu as CM
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Annotation.Menu"

data MenuType = NewNgram | SetTermListItem
derive instance genericMenuType :: Generic MenuType _
instance eqMenuType :: Eq MenuType where
  eq = genericEq

type Props =
  ( list     :: Maybe TermList
  , menuType :: MenuType
  , setList  :: TermList -> Effect Unit -- not a state hook setter
  )

type AnnotationMenu = (
    onClose    :: Effect Unit
  , redrawMenu :: T.Box Boolean
  , x          :: Number
  , y          :: Number
  | Props
  )

type AnnotationMenuWrapper =
  (
    menuRef :: R.Ref (Maybe (Record AnnotationMenu))
  )

eqAnnotationMenu :: Record AnnotationMenu -> Record AnnotationMenu -> Boolean
eqAnnotationMenu new old = new.list == old.list &&
                           new.menuType == old.menuType &&
                           new.x == old.x &&
                           new.y == old.y

eqAnnotationMenuWrapper :: { new :: Maybe (Record AnnotationMenu)
                           , old :: Maybe (Record AnnotationMenu) } -> Effect Boolean
eqAnnotationMenuWrapper { new: Nothing, old: Nothing } = pure $ true
eqAnnotationMenuWrapper { new: Nothing, old: Just _ } = pure $ false
eqAnnotationMenuWrapper { new: Just _, old: Nothing } = pure $ false
eqAnnotationMenuWrapper { new: Just n, old: Just o } = pure $ eqAnnotationMenu n o

annotationMenuWrapper :: R2.Leaf AnnotationMenuWrapper
annotationMenuWrapper p = R.createElement annotationMenuWrapperCpt p []
annotationMenuWrapperCpt :: R.Component AnnotationMenuWrapper
annotationMenuWrapperCpt = here.component "annotationMenuWrapper" cpt where
  cpt { menuRef } _ = do
    case R.readRef menuRef of
      Nothing -> pure $ HTML.div {} []
      Just menu -> pure $ annotationMenu menu

-- | An Annotation Menu is parameterised by a Maybe Termlist of the
-- | TermList the currently selected text belongs to
annotationMenu :: R2.Leaf AnnotationMenu
annotationMenu p = R.createElement annotationMenuCpt p []
annotationMenuCpt :: R.Component AnnotationMenu
annotationMenuCpt = here.component "annotationMenu" cpt where
  cpt { x, y, list, menuType, onClose, redrawMenu, setList } _ = do
    redrawMenu' <- T.useLive T.unequal redrawMenu

    pure $ CM.contextMenu {x, y, onClose} [
        annotationMenuInner { list, menuType, setList }
      ]

annotationMenuInner :: R2.Leaf Props
annotationMenuInner p = R.createElement annotationMenuInnerCpt p []
annotationMenuInnerCpt :: R.Component Props
annotationMenuInnerCpt = here.component "annotationMenuInner" cpt where
  cpt props _ = pure $ R.fragment $ A.mapMaybe (addToList props) [ MapTerm, CandidateTerm, StopTerm ]

-- | Given the TermList to render the item for zand the Maybe TermList the item may belong to, possibly render the menuItem
addToList :: Record Props -> TermList -> Maybe R.Element
addToList {list: Just t'} t
  | t == t'   = Nothing
addToList {menuType, setList} t = Just $ CM.contextMenuItem {} [ link ]
  where
    link = HTML.a { on: { click }, className: className } [ HTML.text (label menuType) ]
    label NewNgram = "Add to " <> termListName t
    label SetTermListItem = "Change to " <> termListName t
    className = "list-group-item list-group-item-" <> (termBootstrapClass t)
    click _ = setList t
