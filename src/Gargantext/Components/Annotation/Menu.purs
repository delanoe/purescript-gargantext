-- | A ContextMenU that allows you to add terms to a list
module Gargantext.Components.Annotation.Menu where


import Prelude ( Unit, (==), ($), (<>), unit, pure )
import Data.Array as A
import Data.Maybe ( Maybe(..), maybe' )
import Effect ( Effect )
import Effect.Uncurried ( mkEffectFn1 )
import Reactix as R
import Reactix.DOM.HTML as HTML
import Reactix.SyntheticEvent as E

import Gargantext.Types ( TermList(..), termListName )
import Gargantext.Components.Annotation.Utils ( termClass )

import Gargantext.Components.ContextMenu.ContextMenu as CM

type Props = ( list :: Maybe TermList )

type AnnotationMenu = { x :: Number, y :: Number, list :: Maybe TermList }

-- | An Annotation Menu is parameterised by a Maybe Termlist of the
-- | TermList the currently selected text belongs to
annotationMenu :: (Maybe AnnotationMenu -> Effect Unit) -> AnnotationMenu -> R.Element
annotationMenu setMenu { x,y,list } =
  CM.contextMenu { x,y,setMenu } [ R.createElement annotationMenuCpt {list} [] ]

annotationMenuCpt :: R.Component Props
annotationMenuCpt = R.hooksComponent "Annotation.Menu" cpt
  where
    cpt { list } _ = pure $ R.fragment $ children list
    children l = A.mapMaybe (\l' -> addToList l' l) [ GraphTerm, CandidateTerm, StopTerm ]

-- | Given the TermList to render the item for zand the Maybe TermList the item may belong to, possibly render the menuItem
addToList :: TermList -> Maybe TermList -> Maybe R.Element
addToList t (Just t')
  | t == t' = Nothing
  | true = addToList t Nothing
addToList t _ = Just $ CM.contextMenuItem [ link ]
  where link = HTML.a { onClick: click, className: className } [ HTML.text label ]
        label = "Add to " <> termListName t
        className = termClass t
        click = mkEffectFn1 $ \_ -> addToTermList t

-- TODO: what happens when we add to a term list?
addToTermList :: TermList -> Effect Unit
addToTermList _ = pure unit

