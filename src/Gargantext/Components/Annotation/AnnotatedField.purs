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
import Data.Maybe ( Maybe(..), maybe )
import Data.Tuple ( Tuple(..) )
import Data.Tuple.Nested ( (/\) )
import DOM.Simple.Console (log2)
import DOM.Simple.Event as DE
import Effect ( Effect )
import Reactix as R
import Reactix.DOM.HTML as HTML
import Reactix.SyntheticEvent as E

import Gargantext.Types (CTabNgramType(..), TermList)
import Gargantext.Components.Annotation.Utils ( termBootstrapClass )
import Gargantext.Components.NgramsTable.Core (NgramsTable, NgramsTerm, findNgramTermList, highlightNgrams, normNgram)
import Gargantext.Components.Annotation.Menu ( AnnotationMenu, annotationMenu, MenuType(..) )
import Gargantext.Utils.Selection as Sel

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
    cpt {ngrams,setTermList,text} _ = do
      mMenu@(_ /\ setMenu) <- R.useState' Nothing

      menuRef <- R.useRef Nothing

      let wrapperProps = { className: "annotated-field-wrapper" }

          onSelect :: String -> Maybe TermList -> MouseEvent -> Effect Unit
          onSelect text' Nothing event = do
            --log2 "[onSelect] text'" text'
            maybeShowMenu setMenu menuRef setTermList ngrams event
          onSelect text' (Just list) event = do
            --log2 "[onSelect] text'" text'
            --log2 "[onSelect] list" (show list)
            let x = E.clientX event
                y = E.clientY event
                setList t = do
                  R.setRef menuRef Nothing
                  setTermList (normNgram CTabTerms text') (Just list) t
                  --setMenu (const Nothing)
                menu = Just {
                    x
                  , y
                  , list: Just list
                  , menuType: SetTermListItem
                  , onClose: \_ -> R.setRef menuRef Nothing
                  , setList
                  }
            setMenu $ const menu
            R.setRef menuRef menu

          mapCompile (Tuple t l) = {text: t, list: l, onSelect}
          compiled = map mapCompile $ compile ngrams text

          runs =
            HTML.div { className: "annotated-field-runs" } $ map annotateRun compiled
      --pure $ HTML.div wrapperProps [maybeAddMenu mMenu runs]
      pure $ HTML.div wrapperProps [ addMenu { menuRef }, runs ]


type AddMenuProps = (
  menuRef :: R.Ref (Maybe AnnotationMenu)
  )


addMenu :: Record AddMenuProps -> R.Element
addMenu p = R.createElement addMenuCpt p []

addMenuCpt :: R.Component AddMenuProps
addMenuCpt = R.hooksComponentWithModule thisModule "addMenu" cpt
  where
    cpt { menuRef } _ = do
      (mMenu /\ setmMenu) <- R.useState' (Nothing :: Maybe AnnotationMenu)

      R.useEffect' $ do
        let m = R.readRef menuRef
        --log2 "[addMenu] menuRef" m
        --log2 "[addMenu] mMenu" mMenu
        setmMenu $ const m

      pure $ case mMenu of
        Nothing -> HTML.div {} []
        Just menu -> annotationMenu setmMenu menu

-- forall e. IsMouseEvent e => R.Setter (Maybe AnnotationMenu) -> R.Setter ? -> ? -> e -> Effect Unit
maybeShowMenu setMenu menuRef setTermList ngrams event = do
  s <- Sel.getSelection
  --log2 "[maybeShowMenu] s" s
  case s of
    Just sel -> do
      case Sel.selectionToString sel of
        "" -> pure unit
        sel' -> do
          let x = E.clientX event
              y = E.clientY event
              n = normNgram CTabTerms sel'
              list = findNgramTermList ngrams n
              setList t = do
                setTermList n list t
                --setMenu (const Nothing)
                R.setRef menuRef Nothing
          E.preventDefault event
          range <- Sel.getRange sel 0
          log2 "[maybeShowMenu] selection range" $ Sel.rangeToTuple range
          let menu = Just {
                x
              , y
              , list
              , menuType: NewNgram
              , onClose: \_ -> R.setRef menuRef Nothing
              , setList
            }
          setMenu $ const $ menu
          R.setRef menuRef menu
    Nothing -> pure unit
    -- Nothing -> do
    --   R.setRef menuRef Nothing

maybeAddMenu
  :: R.State (Maybe AnnotationMenu)
  -> R.Element
  -> R.Element
maybeAddMenu (Just props /\ setMenu) e = annotationMenu setMenu props <> e
maybeAddMenu _ e = e

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
    cpt    { list: Nothing, onSelect, text }     _ =
      HTML.span { on: { mouseUp: \e -> onSelect text Nothing e } } [ HTML.text text ]

    cpt    { list: (Just list), onSelect, text } _ =
      HTML.span { className: className list
                , on: { click: \e -> onSelect text (Just list) e } } [ HTML.text text ]
      where
        className list' = "annotation-run bg-" <> termBootstrapClass list'


