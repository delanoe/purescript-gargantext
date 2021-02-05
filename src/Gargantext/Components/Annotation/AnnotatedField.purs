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
--import DOM.Simple.Console (log2)
import DOM.Simple.Event as DE
import Data.Tuple ( Tuple )
import Data.Tuple.Nested ( (/\) )
import Effect ( Effect )
import Reactix as R
import Reactix.DOM.HTML as HTML
import Reactix.SyntheticEvent as E

import Gargantext.Prelude

import Gargantext.Components.Annotation.Menu ( annotationMenu, AnnotationMenu, MenuType(..) )
import Gargantext.Components.Annotation.Utils (termClass)
import Gargantext.Components.NgramsTable.Core (NgramsTable, NgramsTerm, findNgramTermList, highlightNgrams, normNgram)
import Gargantext.Utils.Selection as Sel
import Gargantext.Types (CTabNgramType(..), TermList)

thisModule :: String
thisModule = "Gargantext.Components.Annotation.AnnotatedField"

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
annotatedFieldComponent = R.hooksComponentWithModule thisModule "annotatedField" cpt
  where
    cpt {ngrams, setTermList, text: fieldText} _ = do
      (_ /\ setRedrawMenu) <- R.useState' false

      menuRef <- R.useRef (Nothing :: Maybe AnnotationMenu)

      let wrapperProps = { className: "annotated-field-wrapper" }

          wrap (text /\ list) = { list
                               , onSelect: onAnnotationSelect { menuRef, ngrams, setRedrawMenu, setTermList }
                               , text }

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

onAnnotationSelect { menuRef, ngrams, setRedrawMenu, setTermList } text mList event =
  case mList of
    Just list ->
      showMenu { event
                , getList: const (Just list)
                , menuRef
                , menuType: SetTermListItem
                , setRedrawMenu
                , setTermList
                , text }
    Nothing -> do
      s <- Sel.getSelection
      case s of
        Just sel -> do
          case Sel.selectionToString sel of
            "" -> hideMenu { menuRef, setRedrawMenu }
            sel' -> do
              showMenu { event
                        , getList: findNgramTermList ngrams
                        , menuRef
                        , menuType: NewNgram
                        , setRedrawMenu
                        , setTermList
                        , text: sel' }
        Nothing -> hideMenu { menuRef, setRedrawMenu }

showMenu { event, getList, menuRef, menuType, setRedrawMenu, setTermList, text } = do
  let x = E.clientX event
      y = E.clientY event
      n = normNgram CTabTerms text
      list = getList n
      redrawMenu = setRedrawMenu not
      setList t = do
        setTermList n list t
        hideMenu { menuRef, setRedrawMenu }
  E.preventDefault event
  --range <- Sel.getRange sel 0
  --log2 "[showMenu] selection range" $ Sel.rangeToTuple range
  let menu = Just
        { x
        , y
        , list
        , menuType
        , onClose: hideMenu { menuRef, setRedrawMenu }
        , setList
        }
  R.setRef menuRef menu
  redrawMenu

hideMenu { menuRef, setRedrawMenu } = do
  let redrawMenu = setRedrawMenu not
  R.setRef menuRef Nothing
  redrawMenu

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
