-- | A ContextMenU that allows you to add terms to a list
module Gargantext.Components.Annotation.Menu where


import Prelude ( Unit, (==), ($), (<>), unit, pure, otherwise, const )
import Data.Array as A
import Data.Maybe ( Maybe(..), maybe' )
import Effect ( Effect )
import Effect.Uncurried ( mkEffectFn1 )
import Reactix as R
import Reactix.DOM.HTML as HTML
import Reactix.SyntheticEvent as E

import Gargantext.Types ( TermList(..), termListName )
import Gargantext.Components.Annotation.Utils ( termBootstrapClass )

import Gargantext.Components.ContextMenu.ContextMenu as CM
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Selection (Selection, selectionToString)

data MenuType = NewNgram | SetTermListItem

type Props =
  ( list :: Maybe TermList
  , menuType :: MenuType
  , setList :: TermList -> Effect Unit -- not a state hook setter
  )

type AnnotationMenu = { x :: Number, y :: Number | Props }

-- | An Annotation Menu is parameterised by a Maybe Termlist of the
-- | TermList the currently selected text belongs to
annotationMenu :: R2.StateSetter (Maybe AnnotationMenu) -> AnnotationMenu -> R.Element
annotationMenu setMenu { x,y,list,menuType,setList } =
  CM.contextMenu { x,y,setMenu } [
    R.createElement annotationMenuCpt {list,menuType,setList} []
  ]

annotationMenuCpt :: R.Component Props
annotationMenuCpt = R.hooksComponent "Annotation.Menu" cpt
  where
    cpt props _ = pure $ R.fragment $ children props
    children props = A.mapMaybe (addToList props) [ GraphTerm, CandidateTerm, StopTerm ]

-- | Given the TermList to render the item for zand the Maybe TermList the item may belong to, possibly render the menuItem
addToList :: Record Props -> TermList -> Maybe R.Element
addToList {list: Just t'} t
  | t == t'   = Nothing
addToList {menuType, setList} t = Just $ CM.contextMenuItem [ link ]
  where
    link = HTML.a { onClick: click, className: className } [ HTML.text (label menuType) ]
    label NewNgram = "Add to " <> termListName t
    label SetTermListItem = "Change to " <> termListName t
    className = "list-group-item list-group-item-" <> (termBootstrapClass t)
    click = mkEffectFn1 $ \_ -> setList t
