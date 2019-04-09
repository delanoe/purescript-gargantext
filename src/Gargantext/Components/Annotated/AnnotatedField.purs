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
import Data.Unit (Unit, unit)
import Data.Array (fromFoldable)
import Data.Maybe (Maybe(..), isJust)
import Data.Lens (Lens', Prism', over, view, lens)
import Data.List as List
import Data.List (List(..), mapWithIndex, toUnfoldable, sortBy)
import Data.Ordering (Ordering(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import React (Children, ReactElement, ReactClass, createElement)
import React.DOM (a, div, p, span, nav, text)
import React.DOM.Props (className, onContextMenu)
import Thermite ( PerformAction, Render, Spec
                , defaultPerformAction, createClass
                , _render, modifyState, focus, focusState
                , simpleSpec, withState)
import Gargantext.Types (TermList(..))
import Gargantext.Components.NgramsTable (NgramsTable, termStyle)
import Gargantext.Utils.React (WithChildren)
import Gargantext.Utils.Selection (getSelection, toString)
import React.SyntheticEvent (SyntheticMouseEvent, pageX, pageY)

newtype PageOffset = PageOffset { x :: Number, y :: Number }

type Run = Tuple String (Maybe TermList)

type State = { runs :: List Run, contextMenu :: { visible :: Boolean } }

type Props' = ( ngrams :: NgramsTable, text :: Maybe String )
type Props = { | Props' }

data Action
  = OnContextMenu PageOffset String
  | AddTerm String TermList

defaultState :: State
defaultState = { runs: Nil, contextMenu: { visible: false } } -- contextMenu: ContextMenu.defaultState }

annotatedField :: Props -> ReactElement
annotatedField p = createElement annotatedFieldClass p []

annotatedFieldClass :: ReactClass (WithChildren Props')
annotatedFieldClass = createClass "AnnotatedField" spec compile
  where
    spec :: Spec State Props Action
    spec = simpleSpec performAction render
    -- performAction (ShowContextMenu i) = showContextMenu i
    -- performAction (AddTerm t l) = addTerm t l
    performAction :: PerformAction State Props Action
    performAction = defaultPerformAction
    render :: Render State Props Action
    render d _p s _c = [ p [className "annotated-field"] $ children d s.runs ]
    children d = fromFoldable <<< map (renderRun $ contextMenuHandler d)
    renderRun menu (Tuple txt lst)
      | Just list <- lst = span [termStyle list, onContextMenu menu] [text txt]
      | otherwise = span [] [text txt]

-- showContextMenu :: PerformAction State Props String
-- showContextMenu p s = pure unit

-- addTerm :: String -> PerformAction State Props TermList
-- addTerm t l p s = pure unit

compile :: Props -> State
compile {text, ngrams} = { runs: runs text, contextMenu: { visible: false } }
  where runs (Just txt) = hilite ngrams txt
        runs _ = Nil

-- highlightNgrams :: NgramsTable -> String -> Array (Tuple String (Maybe TermList))

-- TODO HOOK IN string search
hilite :: NgramsTable -> String -> List Run
hilite _ _ = List.fromFoldable
  [ Tuple "Hello" (Just CandidateTerm)
  , Tuple " " Nothing
  , Tuple "World" (Just CandidateTerm) ]
-- hilite = map tupleRun <<< highlightNgrams

contextMenuHandler :: (Action -> Effect Unit) -> SyntheticMouseEvent -> Effect Unit
contextMenuHandler d e =
  do sel <- getSelection
     case toString <$> sel of
       Just s -> submit s
       Nothing -> pure unit
  where submit s = offset >>= \o -> d $ OnContextMenu o s
        offset =
          do x <- pageX e
             y <- pageY e
             pure $ PageOffset { x, y }


_runs = lens (\a -> a.runs) (\a r -> a { runs = r })
_contextMenu = lens (\a -> a.contextMenu) (\a m -> a { contextMenu = m })
