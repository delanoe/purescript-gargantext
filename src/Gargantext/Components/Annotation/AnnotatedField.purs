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
import Data.Maybe ( Maybe(..), maybe, maybe' )
import Data.Lens ( Lens', lens )
import Data.Traversable ( traverse_ )
import Data.Tuple ( Tuple(..) )
import Data.Tuple.Nested ( (/\) )
import DOM.Simple.Console
import DOM.Simple.Event as DE
import Effect ( Effect )
import Effect.Uncurried (mkEffectFn1)
import Reactix as R
import Reactix.DOM.HTML as HTML
import Reactix.SyntheticEvent as E

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
      menu /\ setMenu <- R.useState $ \_ -> pure Nothing
      let wrapperProps =
            { className: "annotated-field-wrapper"
            , onContextMenu: mkEffectFn1 (maybeShowMenu setMenu props.ngrams) }
      pure $ HTML.div wrapperProps [ maybeAddMenu setMenu (runs props) menu]

maybeAddMenu
  :: (Maybe AnnotationMenu -> Effect Unit)
  -> R.Element
  -> Maybe AnnotationMenu
  -> R.Element
maybeAddMenu setMenu e (Just props) = annotationMenu setMenu props <> e
maybeAddMenu _ e _ = e

compile :: Record Props -> Array Run
compile props = runs props.text
  where runs = maybe [] (highlightNgrams props.ngrams)

maybeShowMenu
  :: forall t
  .  (Maybe AnnotationMenu -> Effect Unit)
  -> NgramsTable
  -> E.SyntheticEvent DE.MouseEvent
  -> Effect Unit
maybeShowMenu setMenu ngrams event = do
  s <- Sel.getSelection
  case s of
    Just sel -> do
      case Sel.selectionToString sel of
        "" -> pure unit
        sel' -> do
          let x = E.clientX event
          let y = E.clientY event
          E.preventDefault event
          setMenu $ Just { x, y, list: findNgram ngrams sel' }
    Nothing -> pure unit

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


