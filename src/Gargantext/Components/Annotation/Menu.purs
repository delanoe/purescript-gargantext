-- | A ContextMenU that allows you to add terms to a list
module Gargantext.Components.Annotation.Menu where


import Prelude (Unit, pure, ($), (<>), (==))
import Data.Array as A
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import Reactix as R
import Reactix.DOM.HTML as HTML

import Gargantext.Types (TermList(..), termListName)
import Gargantext.Components.Annotation.Utils (termBootstrapClass)

import Gargantext.Components.ContextMenu.ContextMenu as CM
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Annotation.Menu"

data MenuType = NewNgram | SetTermListItem

type Props =
  ( list     :: Maybe TermList
  , menuType :: MenuType
  , setList  :: TermList -> Effect Unit -- not a state hook setter
  )

type AnnotationMenu = (
    x       :: Number
  , y       :: Number
  , onClose :: Effect Unit
  | Props
  )

-- | An Annotation Menu is parameterised by a Maybe Termlist of the
-- | TermList the currently selected text belongs to
annotationMenu :: R2.Leaf AnnotationMenu
annotationMenu p = R.createElement annotationMenuCpt p []
annotationMenuCpt :: R.Component AnnotationMenu
annotationMenuCpt = here.component "annotationMenu" cpt where
  cpt { x, y, list, menuType, onClose, setList } _ = do
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
