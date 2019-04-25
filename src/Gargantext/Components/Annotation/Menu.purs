-- | A ContextMenU that allows you to add terms to a list
module Gargantext.Components.Annotation.Menu where


import Prelude ( Unit, (==), ($), (<>), unit, pure )
import Data.Array as A
import Data.Maybe ( Maybe(..) )
import Effect ( Effect )
import Effect.Uncurried ( mkEffectFn1 )
import Reactix as R
import Reactix.DOM.Raw as RDOM
import Unsafe.Coerce ( unsafeCoerce )

import Gargantext.Types ( TermList(..), termListName )
import Gargantext.Utils.Reactix as R'
import Gargantext.Components.Annotation.Utils ( termClass )

import Gargantext.Components.ContextMenu.ContextMenu as CM

type Props = ( termList :: Maybe TermList )

-- | An Annotation Menu is parameterised by a Maybe Termlist of the
-- | TermList the currently selected text belongs to
annotationMenu :: Record Props -> R.Element
annotationMenu = R.createLeaf annotationMenuCpt

annotationMenuCpt :: R.Component Props
annotationMenuCpt = R.hooksLeaf "Annotation.Menu" cpt
  where
    cpt :: forall m. R'.HooksLeaf m Props
    cpt { termList } = pure $
      R'.div { className: "annotation-menu" } [ CM.contextMenu $ children termList ]
    children l = A.mapMaybe (\l' -> addToList l' l) [ GraphTerm, CandidateTerm, StopTerm ]

-- | Given the TermList to render the item for and the Maybe TermList the item may belong to, possibly render the menuItem
addToList :: TermList -> Maybe TermList -> Maybe R.Element
addToList t (Just t')
  | t == t' = Nothing
  | true = addToList t Nothing
addToList t _ = Just $ CM.contextMenuItem [ link ]
  where link = R'.a { onClick: click, className: className } [ RDOM.text label ]
        label = "Add to " <> termListName t
        className = termClass t
        click = mkEffectFn1 $ \_ -> addToTermList t

-- TODO: what happens when we add to a term list?
addToTermList :: TermList -> Effect Unit
addToTermList _ = pure unit

