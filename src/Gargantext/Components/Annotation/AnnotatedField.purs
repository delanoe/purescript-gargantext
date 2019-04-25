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
import Data.Maybe ( Maybe(..), maybe )
import Data.Lens ( Lens', lens )
import Data.Traversable ( traverse_ )
import Data.Tuple ( Tuple(..) )
import Effect ( Effect )
import Reactix as R
import Reactix.DOM.Raw as RDOM
import Reactix.SyntheticEvent as E

import Gargantext.Types ( TermList(..) )
import Gargantext.Components.Annotation.Utils ( termClass )
import Gargantext.Components.NgramsTable ( NgramsTable(..), highlightNgrams )
import Gargantext.Components.Annotation.Menu ( annotationMenu )
import Gargantext.Utils.Selection as Sel

newtype PageOffset = PageOffset { x :: Number, y :: Number }

type Run = Tuple String (Maybe TermList)

type Props = ( ngrams :: NgramsTable, text :: Maybe String )

defaultProps :: Record Props
defaultProps = { ngrams: NgramsTable Map.empty, text: Nothing }

annotatedField :: Record Props -> R.Element
annotatedField p = R.createElement annotatedFieldComponent p []

annotatedFieldComponent :: R.Component Props
annotatedFieldComponent = R.staticComponent "AnnotatedField" cpt
  where
    runs props = annotateRun <$> compile props
    cpt props _ =
      RDOM.div { className: "annotated-field-wrapper" }
        [ annotationMenu { termList: Nothing }
        , RDOM.div { className: "annotated-field-runs" } (annotateRun <$> compile props) ]

type RunProps = ( list :: Maybe TermList, text :: String )

annotateRun :: Run -> R.Element
annotateRun (Tuple text list) = R.createElement annotatedRunComponent { text, list } []

annotatedRunComponent :: R.Component RunProps
annotatedRunComponent = R.staticComponent "AnnotatedRun" cpt
  where cpt { text, list } _ = maybe (unstyled text) (styled text) list
        styled text list = RDOM.span { className: className list } [ RDOM.text text ]
        unstyled text = RDOM.span {} [ RDOM.text text ]
        className list = "annotation-run " <> termClass list

compile :: Record Props -> Array Run
compile props = runs props.text
  where runs (Just text) = highlightNgrams props.ngrams text
        runs _ = []

maybeShowMenu :: E.MouseEvent -> NgramsTable -> (Maybe TermList -> Effect Unit) -> Effect Unit
maybeShowMenu e n a = Sel.getSelection >>= traverse_ (a <<< findNgram n <<< Sel.toString)

-- showMenu


findNgram :: NgramsTable -> String -> Maybe TermList
findNgram _ _ = Nothing

-- contextMenuHandler :: (Action -> Effect Unit) -> MouseEvent -> Effect Unit
-- contextMenuHandler d e =
--   do sel <- getSelection
--      case toString <$> sel of
--        Just s -> submit s
--        Nothing -> pure unit
--   where submit s = offset >>= \o -> d $ OnContextMenu o s
--         offset =
--           do x <- pageX e
--              y <- pageY e
--              pure $ PageOffset { x, y }


-- _runs :: Lens' State (Array Run)
-- _runs = lens (\a -> a.runs) (\a r -> a { runs = r })

-- _contextMenu :: Lens' State ???
-- _contextMenu = lens (\a -> a.contextMenu) (\a m -> a { contextMenu = m })
