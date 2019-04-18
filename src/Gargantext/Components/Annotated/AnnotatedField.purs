-- | The AnnotatedField Component is for colouring ngrams that appear in a text
-- | 
-- | Given a list of ngrams and a text, it:
-- | 
-- | 1. Searches the text for the ngrams
-- | 2. Renders each the resulting runs according to the Maybe TermList they appear in
-- | 
-- | Notes:
-- | 
-- | 1. We must only re-search the text when the ngrams change for performance
-- | 2. We will need a more ambitious search algorithm for skipgrams.
module Gargantext.Components.Annotated.AnnotatedField where

import Prelude hiding (div)
import Data.Array as A
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Lens (Lens', lens)
import Data.List as List
import Data.List (List(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import React (ReactElement, ReactClass, createElement)
import Gargantext.Types (TermList(..))
import Gargantext.Components.NgramsTable (NgramsTable(..), highlightNgrams)
import Gargantext.Utils.Selection (getSelection, toString)
import Reactix as R
import Reactix.DOM.Raw as RDOM

newtype PageOffset = PageOffset { x :: Number, y :: Number }

type Run = Tuple String (Maybe TermList)

type Props = ( ngrams :: NgramsTable, text :: Maybe String )

defaultProps :: Record Props
defaultProps = { ngrams: NgramsTable Map.empty, text: Nothing }

annotatedField :: Record Props -> R.Element
annotatedField = R.createLeaf annotatedFieldComponent

annotatedFieldComponent :: R.Component Props
annotatedFieldComponent = R.pureLeaf "AnnotatedField" cpt
  where
    cpt props = RDOM.p { className: "annotated-field" } $ children props
    children props = A.fromFoldable (annotateRun <$> compile props)

type RunProps = ( list :: Maybe TermList, text :: String )

annotateRun :: Run -> R.Element
annotateRun (Tuple text list) = R.createLeaf annotatedRunComponent { text, list }

annotatedRunComponent :: R.Component RunProps
annotatedRunComponent = R.pureLeaf "AnnotatedRun" cpt
  where cpt { text, list } = maybe (unstyled text) (styled text) list
        styled text list = RDOM.span { style: termStyle list } [ RDOM.text text ]
        unstyled text = RDOM.span {} [ RDOM.text text ]

compile :: Record Props -> List Run
compile props = runs props.text
  where runs (Just text) = highlight props.ngrams text
        runs _ = Nil

highlight :: NgramsTable -> String -> List Run
highlight n t = List.fromFoldable $ highlightNgrams n t

-- contextMenuHandler :: (Action -> Effect Unit) -> SyntheticMouseEvent -> Effect Unit
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

termStyle :: TermList -> { backgroundColor :: String }
termStyle GraphTerm     = { backgroundColor: "green" }
termStyle StopTerm      = { backgroundColor: "red" }
termStyle CandidateTerm = { backgroundColor: "black" }

-- _runs :: Lens' State (List Run)
-- _runs = lens (\a -> a.runs) (\a r -> a { runs = r })

-- _contextMenu :: Lens' State ???
-- _contextMenu = lens (\a -> a.contextMenu) (\a m -> a { contextMenu = m })
