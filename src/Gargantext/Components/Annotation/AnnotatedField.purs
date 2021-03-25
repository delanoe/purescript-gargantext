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
import Data.List ( List(..), (:), length )
import Data.Maybe ( Maybe(..), maybe )
import Data.String.Common ( joinWith )
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ( (/\) )
--import DOM.Simple.Console (log2)
import DOM.Simple.Event as DE
import Effect (Effect)
import Reactix as R
import Reactix.DOM.HTML as HTML
import Reactix.SyntheticEvent as E
import Toestand as T

import Gargantext.Prelude

import Gargantext.Components.Annotation.Menu ( annotationMenu, AnnotationMenu, MenuType(..) )
import Gargantext.Components.Annotation.Utils ( termBootstrapClass, termClass )
import Gargantext.Components.NgramsTable.Core (NgramsTable, NgramsTerm, findNgramTermList, highlightNgrams, normNgram)
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Selection as Sel
import Gargantext.Types (CTabNgramType(..), TermList)

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
annotatedField = R.createElement annotatedFieldComponent

annotatedFieldComponent :: R.Component Props
annotatedFieldComponent = here.component "annotatedField" cpt
  where
    cpt { ngrams, setTermList, text: fieldText } _ = do
      redrawMenu <- T.useBox false

      menuRef <- R.useRef (Nothing :: Maybe AnnotationMenu)

      let wrapperProps = { className: "annotated-field-wrapper" }

          wrap (text /\ list) = { list
                               , onSelect: onAnnotationSelect { menuRef, ngrams, redrawMenu, setTermList }
                               , text }

      pure $ HTML.div wrapperProps
        [ maybe (HTML.div {} []) annotationMenu $ R.readRef menuRef
        , HTML.div { className: "annotated-field-runs" }
             ((\p -> annotateRun p []) <$> wrap <$> compile ngrams fieldText)
        ]

compile :: NgramsTable -> Maybe String -> Array (Tuple String (List (Tuple NgramsTerm TermList)))
compile ngrams = maybe [] (highlightNgrams CTabTerms ngrams)

-- Runs

onAnnotationSelect :: forall e. DE.IsMouseEvent e => { menuRef :: R.Ref (Maybe AnnotationMenu)
                                              , ngrams :: NgramsTable
                                              , redrawMenu :: T.Box Boolean
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
onAnnotationSelect { menuRef, ngrams, redrawMenu, setTermList } (Just (Tuple ngram list)) event =
  showMenu { event
           , getList: const (Just list)
           , menuRef
           , menuType: SetTermListItem
           , ngram
           , redrawMenu
           , setTermList }

-- showMenu :: forall p e. DE.IsMouseEvent e => { event :: E.SyntheticEvent e | p } -> Effect Unit
showMenu :: forall e. DE.IsMouseEvent e => { event :: E.SyntheticEvent e
                                    , getList :: NgramsTerm -> Maybe TermList
                                    , menuRef :: R.Ref (Maybe AnnotationMenu)
                                    , menuType :: MenuType
                                    , ngram :: NgramsTerm
                                    , redrawMenu :: T.Box Boolean
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
  E.preventDefault event
  --range <- Sel.getRange sel 0
  --log2 "[showMenu] selection range" $ Sel.rangeToTuple range
  let menu = Just
        { list
        , onClose: hideMenu { menuRef, redrawMenu }
        , menuType
        , setList
        , x
        , y }
  R.setRef menuRef menu
  T.modify_ not redrawMenu

hideMenu { menuRef, redrawMenu } = do
  R.setRef menuRef Nothing
  T.modify_ not redrawMenu

type Run =
  ( list :: List (Tuple NgramsTerm TermList)
  , onSelect :: Maybe (Tuple NgramsTerm TermList) -> MouseEvent -> Effect Unit
  , text :: String
  )

annotateRun :: R2.Component Run
annotateRun = R.createElement annotatedRunComponent

annotatedRunComponent :: R.Component Run
annotatedRunComponent = R.staticComponent "AnnotatedRun" cpt
  where
    cpt    { list: Nil, onSelect, text }     _ =
      HTML.span { on: { mouseUp: onSelect Nothing } } [ HTML.text text ]

    cpt    { list: lst@((ngram /\ list) : otherLists), onSelect, text } _ =
      HTML.span { className
                , on: { click: onSelect (Just (ngram /\ list)) } } [ HTML.text text ]
      where
        bgClasses = joinWith " " $ A.fromFoldable $ termClass <<< snd <$> lst
        -- className = "annotation-run bg-" <> termBootstrapClass list
        className = "annotation-run " <> bgClasses
        -- cb = onSelect text list
        -- elt =
        --   case list of
        --      Nothing -> HTML.span { on: { mouseUp: cb } }
        --      Just l  -> HTML.span { -- className: "annotation-run bg-" <> termBootstrapClass l
        --                             className: "annotation-run " <> termClass l
        --                           , on: { click: cb }
        --                           }
