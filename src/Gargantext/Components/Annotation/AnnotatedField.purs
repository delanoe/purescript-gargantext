-- | The AnnotatedField Component is for colouring ngrams that appear in a text
-- | 
-- | Given an array of ngrams and a text, it:
-- | 
-- | 1. Searches the text for the ngrams
-- | 2. Renders each the resulting runs according to the Maybe TermList they appear in
-- | 
-- | Notes:
-- | 
-- | 1. We must only re-search the text when the ngrams change for performance
-- | 2. We will need a more ambitious search algorithm for skipgrams.
module Gargantext.Components.Annotation.AnnotatedField where

import Prelude
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, maybe', fromMaybe)
import Data.Lens (Lens', lens)
import Data.Nullable (Nullable, null, toMaybe)
import Data.Ord (max)
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import DOM.Simple as DOM
import DOM.Simple.Console
import DOM.Simple.Document ( document )
import DOM.Simple.Document as Document
import DOM.Simple.Node as Node
import DOM.Simple.Types (Element)
import DOM.Simple.Element as Element
import DOM.Simple.Event as DE
import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Reactix as R
import Reactix.DOM.HTML as HTML

import Gargantext.Types ( TermList(..) )
import Gargantext.Components.Annotation.Utils ( termClass )
import Gargantext.Components.NgramsTable ( NgramsTable(..), highlightNgrams )
import Gargantext.Components.ContextMenu.ContextMenu as CM
import Gargantext.Components.Annotation.Menu ( AnnotationMenu, annotationMenu )
import Gargantext.Utils.Selection as Sel

type Run = Tuple String (Maybe TermList)

type Props = ( ngrams :: NgramsTable, text :: Maybe String )

defaultProps :: Record Props
defaultProps = { ngrams: NgramsTable Map.empty, text: Nothing }

annotatedField :: Record Props -> R.Element
annotatedField p = R.createElement annotatedFieldComponent p []

annotatedFieldComponent :: R.Component Props
annotatedFieldComponent = R.hooksComponent "AnnotatedField" cpt
  where
    runs props =
      HTML.div { className: "annotated-field-runs" } (map annotateRun $ compile props)
    cpt props _ = do
      menu <- R.useState $ \_ -> pure Nothing
      root <- R.useRef null
      useAnnotationEffect root menu props.ngrams
      pure $ HTML.div { className } [ maybeAddMenu menu (runs props) ]
    className = "annotated-field-wrapper"

useAnnotationEffect
  :: R.Ref (Nullable Element)
  -> R.State (Maybe AnnotationMenu)
  -> NgramsTable
  -> R.Hooks Unit
useAnnotationEffect rootRef menu ngrams =
  R.useLayoutEffect1 (R.readNullableRef rootRef) h
  where
    h _ =
      case R.readNullableRef rootRef of
        Just root -> do
          let handler = onSelectionChange root menu ngrams
          DOM.addEventListener document "selectionchange" handler
          pure $ \_ -> DOM.removeEventListener document "selectionchange" handler
        Nothing -> pure $ \_ -> pure unit

-- | TODO: handle multiple ranges
onSelectionChange
  :: Element
  -> R.State (Maybe AnnotationMenu)
  -> NgramsTable
  -> Element.Callback DE.SelectionEvent
onSelectionChange root (_ /\ setMenu) ngrams =
  Element.callback $ \event ->
    Sel.getSelection >>= traverse (getMenu event) >>= traverse_ setMenu
  where
    getMenu event sel = getMenu' event sel (Sel.selectionToString sel)
    getMenu' event sel sel'
      | not (selEmpty sel sel') = do
        range <- Sel.getRange sel 0
        if not (liesWithin $ Sel.commonAncestorContainer range)
           then pure Nothing
           else do
             let rect = Sel.boundingRect range
             DE.preventDefault event
             -- top and right are the most pessimistic values because the menu is biased
             -- towards being positioned above and to the right of the cursor
             pure $ Just { x: rect.top, y: rect.right, list: findNgram ngrams sel' }
      | true = pure Nothing
    liesWithin = Element.contains root
    selEmpty _ "" = true
    selEmpty sel _ = Sel.rangeCount sel > 0 && not (Sel.isSelectionCollapsed sel)

maybeAddMenu
  :: R.State (Maybe AnnotationMenu)
  -> R.Element
  -> R.Element
maybeAddMenu ((Just props) /\ setMenu) e = annotationMenu setMenu props <> e
maybeAddMenu _ e = e

compile :: Record Props -> Array Run
compile props = runs props.text
  where runs = maybe [] (highlightNgrams props.ngrams)

findNgram :: NgramsTable -> String -> Maybe TermList
findNgram _ _ = Nothing

-- Runs

type RunProps = ( list :: Maybe TermList, text :: String )

annotateRun :: Run -> R.Element
annotateRun (Tuple text list) = R.createElement annotatedRunComponent { text, list } []

annotatedRunComponent :: R.Component RunProps
annotatedRunComponent = R.staticComponent "AnnotatedRun" cpt
  where cpt    { text, list } _ = maybe' (\_ -> unstyled text) (styled text) list
        styled   text  list     = HTML.span { className: className list } [ HTML.text text ]
        unstyled text           = HTML.span {} [ HTML.text text ]
        className      list     = "annotation-run " <> termClass list


