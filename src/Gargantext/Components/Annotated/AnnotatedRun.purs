-- | The AnnotatedRun Component renders an ngram run with appropriate
-- | highlighting and tooltip
module Gargantext.Components.Annotated.AnnotatedRun where

import Prelude hiding (div)
import Data.Maybe (Maybe(..))
import Data.Unit (Unit, unit)
import Data.Array as A
import Data.Lens (Lens', Prism', over, view)
import Data.List (List, mapWithIndex, toUnfoldable, sortBy)
import Data.Ordering (Ordering(..))
import Data.String.Common (joinWith)
import Data.String.CodeUnits as CU
import Data.String.Regex as Regex
import Data.Tuple (Tuple(..))
import React (ReactElement)
import React.DOM (span, text)
import React.DOM.Props (Props, className, title, style, onClick)
import Thermite ( PerformAction, Render, Spec
                , _render, modifyState, focus
                , simpleSpec, withState)
import Gargantext.Types (Term(..), TermList(..), termListName)
import Gargantext.Utils.Reactil (wrap)

-- TODO: Support multiple lists (see the large quantities of commented out code)
-- names :: forall f. Foldable f, => f String -> Array String
-- names = map \(OccurrenceList o) -> o.name

-- tooltip :: Array String -> Maybe String
-- tooltip = help . A.fromFoldable
--   where help [] = Nothing
--         help names = Just $ prelude <> joinWith ", " names
--     where prelude = "Occurs in " <> (show $ A.length names) <> " lists: "

-- wrapColour :: Colour -> ReactElement -> ReactElement
-- wrapColour colour = wrap $ div [color (show colour), opacity "0.8"]

newtype Run = Run { text :: String, list :: Maybe TermList } -- lists :: Array TermList }


-- TODO: Context Menu
-- | A Blank Run is not part an n-gram, but it does have a context menu
renderBlank :: Run -> ReactElement
renderBlank = wrap $ span [className "run run-blank"] <<< text

-- TODO: Context Menu
renderHilite :: Run -> ReactElement
renderHilite r = span [className "run run-pretty", title tooltip] [text r.text]
-- (wrap wrapping) $ foldl wrapColour head r.lists
  where tooltip = "Occurs in the " <> termListName r.list
        -- lists = names r.lists
        -- head = span [className "run run-pretty", title (tooltip lists)] [text r.text]
        -- wrapping = span [className "run run-pretty"]

render :: Run -> ReactElement
render r
  | r.lists == [] = renderBlank r.text
  | otherwise = renderHilite r

data Action = AddTerm Term | RemoveTerm Term

termStyle :: TermList -> Props
termStyle GraphTerm     = style {color: "green"}
termStyle StopTerm      = style {color: "red", textDecoration : "line-through"}
termStyle CandidateTerm = style {color: "black"}

-- annotatedRun ::
  
--   Spec Unit Run Action

