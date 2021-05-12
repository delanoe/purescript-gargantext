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

import Data.Array as A
import Data.List (List(..), (:))
import Data.Maybe ( Maybe(..), maybe )
import Data.String.Common ( joinWith )
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ( (/\) )
import DOM.Simple.Event as DE
import Effect (Effect)
import Reactix as R
import Reactix.DOM.HTML as HTML
import Reactix.SyntheticEvent as E
import Record as Record
import Toestand as T

import Gargantext.Prelude

import Gargantext.Components.Annotation.Menu ( annotationMenuWrapper, AnnotationMenu, MenuType(..) )
import Gargantext.Components.Annotation.Utils (termClass)
import Gargantext.Components.NgramsTable.Core (NgramsTable, NgramsTerm, findNgramTermList, highlightNgrams, normNgram)
import Gargantext.Types (CTabNgramType(..), TermList)
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Selection as Sel

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

annotatedField :: R2.Component Props
annotatedField = R.createElement annotatedFieldCpt
annotatedFieldCpt :: R.Component Props
annotatedFieldCpt = here.component "annotatedField" cpt where
  cpt props _ = do
    menuRef <- R.useRef (Nothing :: Maybe (Record AnnotationMenu))
    redrawMenu <- T.useBox false

    pure $ annotatedFieldInner (Record.merge { menuRef, redrawMenu } props)

type InnerProps =
  (
    menuRef    :: R.Ref (Maybe (Record AnnotationMenu))
  , redrawMenu :: T.Box Boolean
  | Props
  )

annotatedFieldInner :: R2.Leaf InnerProps
annotatedFieldInner p = R.createElement annotatedFieldInnerCpt p []
annotatedFieldInnerCpt :: R.Component InnerProps
annotatedFieldInnerCpt = here.component "annotatedFieldInner" cpt where
  cpt { menuRef, ngrams, redrawMenu, setTermList, text: fieldText } _ = do
    -- redrawMenu <- T.useBox false
    redrawMenu' <- T.useLive T.unequal redrawMenu

    -- menuRef <- R.useRef (Nothing :: Maybe (Record AnnotationMenu))
    -- menu <- T.useBox (Nothing :: Maybe (Record AnnotationMenu))

    let wrap (text /\ list) = { list
                              , onSelect: onAnnotationSelect { menuRef, ngrams, redrawMenu, setTermList }
                              , text }

    pure $ HTML.div { className: "annotated-field-wrapper" }
      [ annotationMenuWrapper { menuRef }
      , HTML.div { className: "annotated-field-runs" }
            ((\p -> annotateRun p []) <$> wrap <$> compile ngrams fieldText)
      ]

compile :: NgramsTable -> Maybe String -> Array (Tuple String (List (Tuple NgramsTerm TermList)))
compile ngrams = maybe [] (highlightNgrams CTabTerms ngrams)

-- Runs

onAnnotationSelect :: forall e. DE.IsMouseEvent e => { menuRef     :: R.Ref (Maybe (Record AnnotationMenu))
                                              , ngrams      :: NgramsTable
                                              , redrawMenu  :: T.Box Boolean
                                              , setTermList :: NgramsTerm -> Maybe TermList -> TermList -> Effect Unit }
                      -> Maybe (Tuple NgramsTerm TermList) -> E.SyntheticEvent e -> Effect Unit
onAnnotationSelect { menuRef, ngrams, redrawMenu, setTermList } Nothing event = do
  s <- Sel.getSelection
  case s of
    Just sel -> do
      case Sel.selectionToString sel of
        "" -> hideMenu { menuRef, redrawMenu }
        sel' -> do
          showMenu { event
                   , getList: findNgramTermList ngrams
                   , menuRef
                   , menuType: NewNgram
                   , ngram: normNgram CTabTerms sel'
                   , redrawMenu
                   , setTermList }
    Nothing -> hideMenu { menuRef, redrawMenu }
onAnnotationSelect { menuRef, ngrams, redrawMenu, setTermList } (Just (Tuple ngram list)) event = do
  showMenu { event
           , getList: const (Just list)
           , menuRef
           , menuType: SetTermListItem
           , ngram
           , redrawMenu
           , setTermList }

-- showMenu :: forall p e. DE.IsMouseEvent e => { event :: E.SyntheticEvent e | p } -> Effect Unit
showMenu :: forall e. DE.IsMouseEvent e => { event       :: E.SyntheticEvent e
                                    , getList     :: NgramsTerm -> Maybe TermList
                                    , menuRef     :: R.Ref (Maybe (Record AnnotationMenu))
                                    , menuType    :: MenuType
                                    , ngram       :: NgramsTerm
                                    , redrawMenu  :: T.Box Boolean
                                    , setTermList :: NgramsTerm -> Maybe TermList -> TermList -> Effect Unit }
            -> Effect Unit
showMenu { event, getList, menuRef, menuType, ngram, redrawMenu, setTermList } = do
  let x = E.clientX event
      y = E.clientY event
      -- n = normNgram CTabTerms text
      list = getList ngram
      -- redrawMenu = T.modify not redrawMenu
      setList t = do
        setTermList ngram list t
        hideMenu { menuRef, redrawMenu }
  -- here.log2 "x" x
  -- here.log2 "y" y
  E.preventDefault event
  --range <- Sel.getRange sel 0
  --here.log2 "selection range" $ Sel.rangeToTuple range
  let menu = Just
        { list
        , menuType
        , onClose: hideMenu { menuRef, redrawMenu }
        , redrawMenu
        , setList
        , x
        , y }
  R.setRef menuRef menu
  T.modify_ not redrawMenu

hideMenu { menuRef, redrawMenu } = do
  R.setRef menuRef Nothing
  T.modify_ not redrawMenu

type Run =
  ( list       :: List (Tuple NgramsTerm TermList)
  , onSelect   :: Maybe (Tuple NgramsTerm TermList) -> MouseEvent -> Effect Unit
  , text       :: String
  )

annotateRun :: R2.Component Run
annotateRun = R.createElement annotatedRunCpt
annotatedRunCpt :: R.Component Run
annotatedRunCpt = here.component "annotatedRun" cpt
  where
    cpt { list, onSelect, text } _ = do

      let el = case list of
            Nil -> HTML.span { on: { mouseUp: onSelect Nothing } } [ HTML.text text ]
            lst@(( ngram /\ list' ) : otherLists) ->
              let bgClasses = joinWith " " $ A.fromFoldable $ termClass <<< snd <$> lst
                  className = "annotation-run " <> bgClasses
              in
              HTML.span { className
                        , on: { click: onSelect (Just (ngram /\ list')) } } [ HTML.text text ]

      pure $ el
