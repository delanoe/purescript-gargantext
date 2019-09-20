-- | The modal component sits atop everything else.  It darkens the
-- | rest of the page and centers a box in which we can put
-- | content. Clicking outside of the box will close the modal
module Gargantext.Components.Modal where

import Prelude hiding (div)
import Data.Maybe ( Maybe(..), maybe )
import Data.Nullable ( Nullable, null, toMaybe )
import Data.Tuple ( Tuple(..) )
import Data.Tuple.Nested ( (/\) )
import DOM.Simple as DOM
import DOM.Simple.Console
import DOM.Simple.EventListener ( Callback, callback )
import DOM.Simple.Element as Element
import DOM.Simple.Event (MouseEvent, target)
import DOM.Simple.Document ( document )
import Effect (Effect)
import Effect.Uncurried ( mkEffectFn1 )
import FFI.Simple ( (...), (..), delay )
import Reactix as R
import Reactix.DOM.HTML as H
import Reactix.SyntheticEvent as E
import Gargantext.Utils.Reactix as R2

type Props = ( setVisible :: R2.Setter Boolean )

modal :: Record Props -> Array R.Element -> R.Element
modal = R.createElement modalCpt

modalCpt :: R.Component Props
modalCpt = R.hooksComponent "Modal" cpt
  where
    cpt {setVisible} children = do
      host <- R2.getPortalHost
      root <- R.useRef null -- used to close when user clicks outside
      R2.useLayoutEffectOnce $ modalEffect root setVisible
      pure $ R.createPortal
        [ H.div { ref: root, className: "modal", data: {toggle: "popover", placement: "right"}}
          [ H.div { className: "popover-content" }
            [ H.div { className: "panel panel-default" }
              [ H.ul { className: "list-group" } children ]]]]
        host

modalEffect
  :: R.Ref (Nullable DOM.Element)
  -> R2.Setter Boolean
  -> Effect (Effect Unit)
modalEffect rootRef setVisible = maybe (pure R.nothing) withRoot (R.readNullableRef rootRef)
  where
    onScroll = R2.named "hideModalOnScroll" $ callback handler
      where -- removing this type declaration will unleash the hounds, so don't
        handler :: MouseEvent -> Effect Unit
        handler _ = setVisible (const false)
    withRoot root = do
      let onClick = clickHandler root
      DOM.addEventListener document "click" onClick
      DOM.addEventListener document "scroll" onScroll
      pure $ do
        DOM.removeEventListener document "click" onClick
        DOM.removeEventListener document "scroll" onScroll
    clickHandler root =
      R2.named "hideModalOnClickOutside" $ callback handler
        where -- removing this type declaration will unleash the hounds, so don't
          handler :: MouseEvent -> Effect Unit
          handler e =
            if Element.contains root (target e)
            then pure unit
            else setVisible (const false)
