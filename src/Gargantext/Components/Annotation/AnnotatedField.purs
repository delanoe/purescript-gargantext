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
import Data.Lens ((^?), _Just)
import Data.Lens.At (at)
import Data.Maybe ( Maybe(..), maybe, maybe' )
import Data.Tuple ( Tuple(..) )
import Data.Tuple.Nested ( (/\) )
import DOM.Simple.Console ( log, log2 )
import DOM.Simple.Event as DE
import Effect.Class ( liftEffect )
import Effect ( Effect )
import Effect.Uncurried ( mkEffectFn1 )
import Reactix as R
import Reactix.DOM.HTML as HTML
import Reactix.SyntheticEvent as E

import Gargantext.Types ( TermList )
import Gargantext.Components.Annotation.Utils ( termClass )
import Gargantext.Components.NgramsTable.Core ( NgramsTerm, NgramsTable(..), _NgramsElement, _list, highlightNgrams )
import Gargantext.Components.Annotation.Menu ( AnnotationMenu, annotationMenu, MenuType(..) )
import Gargantext.Utils.Selection as Sel

type Props =
  ( ngrams       :: NgramsTable
  , setTermList  :: NgramsTerm -> Maybe TermList -> TermList -> Effect Unit
  , text         :: Maybe String
  )

-- UNUSED
-- defaultProps :: Record Props
-- defaultProps = { ngrams: NgramsTable Map.empty, text: Nothing, setTermList: \_ _ _ -> pure unit }

annotatedField :: Record Props -> R.Element
annotatedField p = R.createElement annotatedFieldComponent p []

annotatedFieldComponent :: R.Component Props
annotatedFieldComponent = R.hooksComponent "AnnotatedField" cpt
  where
    cpt {ngrams,setTermList,text} _ = do
      menu /\ setMenu <- R.useState $ \_ -> pure (Nothing :: Maybe AnnotationMenu)
      let wrapperProps =
            { className: "annotated-field-wrapper" }

          onSelect _ Nothing event = maybeShowMenu text setMenu setTermList event
          onSelect text' (Just list) event = do
            let x = E.clientX event
                y = E.clientY event
                setList = setTermList text' (Just list)
            setMenu $ Just {x, y, text: text', list, menuType: SetTermListItem, setList}

          runs =
            HTML.div { className: "annotated-field-runs" } $ map annotateRun (compile ngrams text) onSelect
      pure $ HTML.div wrapperProps [maybeAddMenu setMenu runs menu]

maybeShowMenu text setMenu setTermList event = do
  s <- Sel.getSelection
  case s of
    Just sel -> do
      case Sel.selectionToString sel of
        "" -> pure unit
        sel' -> do
          let x = E.clientX event
              y = E.clientY event
              list = findNgram text sel'
              setList = setTermList sel' list
          E.preventDefault event
          setMenu $ Just { x, y, sel, list, menuType: NewNgram, setList }
    Nothing -> pure unit

maybeAddMenu
  :: (Maybe AnnotationMenu -> Effect Unit)
  -> R.Element
  -> Maybe AnnotationMenu
  -> R.Element
maybeAddMenu setMenu e (Just props) = annotationMenu setMenu props <> e
maybeAddMenu _ e _ = e

--compile :: NgramsTable -> Maybe String -> Array Run
compile ngrams = maybe [] (highlightNgrams ngrams)

findNgram :: NgramsTable -> String -> Maybe TermList
findNgram (NgramsTable m) s = m ^? at s <<< _Just <<< _NgramsElement <<< _list

-- Runs

type Run =
  ( text :: String
  , list :: (Maybe TermList)
  --, onSelect :: a
  )

annotateRun :: Record Run -> R.Element
annotateRun {text, list, onSelect} = R.createElement annotatedRunComponent { text, list, onSelect } []

type RunProps =
  ( text :: String
  , list :: Maybe TermList
            --, onSelect :: a
  )

annotatedRunComponent :: R.Component RunProps
annotatedRunComponent = R.staticComponent "AnnotatedRun" cpt
  where
    cpt    { text, list: Nothing, onSelect }     _ =
      HTML.span { onMouseUp: mkEffectFn1 $ \e -> onSelect text Nothing e } [ HTML.text text ]

    cpt    { text, list: (Just list), onSelect } _ =
      HTML.span { className: className list
                , onClick: mkEffectFn1 $ \e -> onSelect text (Just list) e
                } [ HTML.text text ]
      where
        className     list        = "annotation-run " <> termClass list


