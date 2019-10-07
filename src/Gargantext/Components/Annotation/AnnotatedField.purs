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
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import DOM.Simple.Event as DE
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1)
import Reactix as R
import Reactix.DOM.HTML as HTML
import Reactix.SyntheticEvent as E
import Gargantext.Types (CTabNgramType(..), TermList)
import Gargantext.Components.Annotation.Utils (termBootstrapClass)
import Gargantext.Components.NgramsTable.Core (NgramsTable, NgramsTerm, findNgramTermList, highlightNgrams)
import Gargantext.Components.Annotation.Menu (AnnotationMenu, annotationMenu, MenuType(..))
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Selection as Sel

type Props
  = ( ngrams :: NgramsTable
    , setTermList :: NgramsTerm -> Maybe TermList -> TermList -> Effect Unit
    , text :: Maybe String
    )

type MouseEvent
  = E.SyntheticEvent DE.MouseEvent

-- UNUSED
-- defaultProps :: Record Props
-- defaultProps = { ngrams: NgramsTable Map.empty, text: Nothing, setTermList: \_ _ _ -> pure unit }
annotatedField :: Record Props -> R.Element
annotatedField p = R.createElement annotatedFieldComponent p []

annotatedFieldComponent :: R.Component Props
annotatedFieldComponent = R.hooksComponent "AnnotatedField" cpt
  where
  cpt { ngrams, setTermList, text } _ = do
    menu /\ setMenu <- R.useState $ const Nothing
    let
      wrapperProps = { className: "annotated-field-wrapper" }

      onSelect _ Nothing event = maybeShowMenu setMenu setTermList ngrams event

      onSelect text' (Just list) event = do
        let
          x = E.clientX event

          y = E.clientY event

          setList t = do
            setTermList text' (Just list) t
            setMenu (const Nothing)
        setMenu (const $ Just { x, y, list: Just list, menuType: SetTermListItem, setList })

      mapCompile (Tuple t l) = { text: t, list: l, onSelect }

      compiled = map mapCompile $ compile ngrams text

      runs = HTML.div { className: "annotated-field-runs" } $ map annotateRun compiled
    pure $ HTML.div wrapperProps [ maybeAddMenu setMenu runs menu ]

-- forall e. IsMouseEvent e => R2.Setter (Maybe AnnotationMenu) -> R2.Setter ? -> ? -> e -> Effect Unit
maybeShowMenu setMenu setTermList ngrams event = do
  s <- Sel.getSelection
  case s of
    Just sel -> do
      case Sel.selectionToString sel of
        "" -> pure unit
        sel' -> do
          let
            x = E.clientX event

            y = E.clientY event

            list = findNgramTermList CTabTerms ngrams sel'

            setList t = do
              setTermList sel' list t
              setMenu (const Nothing)
          E.preventDefault event
          setMenu (const $ Just { x, y, list, menuType: NewNgram, setList })
    Nothing -> pure unit

maybeAddMenu ::
  R2.Setter (Maybe AnnotationMenu) ->
  R.Element ->
  Maybe AnnotationMenu ->
  R.Element
maybeAddMenu setMenu e (Just props) = annotationMenu setMenu props <> e

maybeAddMenu _ e _ = e

compile :: NgramsTable -> Maybe String -> Array (Tuple String (Maybe TermList))
compile ngrams = maybe [] (highlightNgrams CTabTerms ngrams)

-- Runs
type Run
  = ( text :: String
    , list :: (Maybe TermList)
    , onSelect :: String -> Maybe TermList -> MouseEvent -> Effect Unit
    )

annotateRun :: Record Run -> R.Element
annotateRun p = R.createElement annotatedRunComponent p []

annotatedRunComponent :: R.Component Run
annotatedRunComponent = R.staticComponent "AnnotatedRun" cpt
  where
  cpt { text, list: Nothing, onSelect } _ = HTML.span { onMouseUp: mkEffectFn1 $ \e -> onSelect text Nothing e } [ HTML.text text ]

  cpt { text, list: (Just list), onSelect } _ =
    HTML.span
      { className: className list
      , onClick: mkEffectFn1 $ \e -> onSelect text (Just list) e
      }
      [ HTML.text text ]
    where
    className list' = "annotation-run bg-" <> termBootstrapClass list'
