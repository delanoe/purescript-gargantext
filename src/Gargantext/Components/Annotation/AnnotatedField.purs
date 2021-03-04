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

import Data.Maybe ( Maybe(..), maybe )
import Data.Tuple ( Tuple )
import Data.Tuple.Nested ( (/\) )
--import DOM.Simple.Console (log2)
import DOM.Simple.Event as DE
import Effect ( Effect )
import Reactix as R
import Reactix.DOM.HTML as HTML
import Reactix.SyntheticEvent as E

import Gargantext.Prelude

import Gargantext.Types (CTabNgramType(..), TermList)
import Gargantext.Components.Annotation.Utils ( termBootstrapClass, termClass )
import Gargantext.Components.NgramsTable.Core
import Gargantext.Components.Annotation.Menu ( annotationMenu, MenuType(..) )
import Gargantext.Utils.Selection as Sel
import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Annotation.AnnotatedField"

type Props =
  ( ngrams       :: NgramsTable
  , setTermList  :: NgramsTerm -> Maybe TermList -> TermList -> Effect Unit
  , text         :: Maybe String
  )
type MouseEvent = E.SyntheticEvent DE.MouseEvent

-- UNUSED
-- defaultProps :: Record Props
-- defaultProps = { ngrams: NgramsTable Map.empty, text: Nothing, setTermList: \_ _ _ -> pure unit }

annotatedField :: Record Props -> R.Element
annotatedField p = R.createElement annotatedFieldComponent p []

annotatedFieldComponent :: R.Component Props
annotatedFieldComponent = here.component "annotatedField" cpt
  where
    cpt {ngrams,setTermList,text: fieldText} _ = do
      (_ /\ setRedrawMenu) <- R.useState' false

      menuRef <- R.useRef Nothing

      let wrapperProps = { className: "annotated-field-wrapper" }

          redrawMenu = setRedrawMenu not

          hideMenu = do
            R.setRef menuRef Nothing
            redrawMenu

          showMenu { event, text, getList, menuType } = do
            let x = E.clientX event
                y = E.clientY event
                n = normNgram CTabTerms text
                list = getList n
                setList t = do
                  setTermList n list t
                  hideMenu
            E.preventDefault event
            --range <- Sel.getRange sel 0
            --log2 "[showMenu] selection range" $ Sel.rangeToTuple range
            let menu = Just
                  { x
                  , y
                  , list
                  , menuType
                  , onClose: hideMenu
                  , setList
                  }
            R.setRef menuRef menu
            redrawMenu

          onSelect :: String -> Maybe TermList -> MouseEvent -> Effect Unit
          onSelect text mList event =
            case mList of
              Just list ->
                showMenu { event, text, getList: const (Just list), menuType: SetTermListItem }
              Nothing -> do
                s <- Sel.getSelection
                case s of
                  Just sel -> do
                    case Sel.selectionToString sel of
                      "" -> hideMenu
                      sel' -> do
                        showMenu { event, text: sel', getList: findNgramTermList ngrams, menuType: NewNgram }
                  Nothing -> hideMenu

          wrap (text /\ list) = {text, list, onSelect}

      pure $ HTML.div wrapperProps
        [ maybe (HTML.div {} []) annotationMenu $ R.readRef menuRef
        , HTML.div { className: "annotated-field-runs" }
             $ annotateRun
            <$> wrap
            <$> compile ngrams fieldText
        ]

compile :: NgramsTable -> Maybe String -> Array (Tuple String (Maybe TermList))
compile ngrams = maybe [] (highlightNgrams CTabTerms ngrams)

-- Runs

type Run =
  ( list :: (Maybe TermList)
  , onSelect :: String -> Maybe TermList -> MouseEvent -> Effect Unit
  , text :: String
  )

annotateRun :: Record Run -> R.Element
annotateRun p = R.createElement annotatedRunComponent p []

annotatedRunComponent :: R.Component Run
annotatedRunComponent = R.staticComponent "AnnotatedRun" cpt
  where
    cpt { list, onSelect, text } _ = elt [ HTML.text text ]
      where
        cb = onSelect text list
        elt =
          case list of
             Nothing -> HTML.span { on: { mouseUp: cb } }
             Just l  -> HTML.span { -- className: "annotation-run bg-" <> termBootstrapClass l
                                    className: "annotation-run " <> termClass l
                                  , on: { click: cb } 
                                  }
