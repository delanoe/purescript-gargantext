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
module Gargantext.Components.Annotation.Field
  ( annotatedField
  )
  where

import Gargantext.Prelude

import DOM.Simple.Event as DE
import Data.Array as A
import Data.List (List(..), (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Gargantext.Components.Annotation.Menu (annotationMenu, AnnotationMenu)
import Gargantext.Components.Annotation.Types (MenuType(..), ModeType(..), termClass)
import Gargantext.Core.NgramsTable.Functions (Cache, findNgramTermList, highlightNgrams, normNgram)
import Gargantext.Core.NgramsTable.Types (HighlightElement, NgramsRepoElement(..), NgramsTable(..), NgramsTerm(..), parentMap)
import Gargantext.Types (CTabNgramType(..), TermList)
import Gargantext.Utils.Reactix as R2
import Gargantext.Utils.Selection as Sel
import Gargantext.Utils.Toestand as T2
import Reactix as R
import Reactix.DOM.HTML as H
import Reactix.SyntheticEvent as E
import Record as Record
import Record.Extra as RX
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.Annotation.Field"

-- @NOTE #386: add parameter "type" ("Authors", "Terms")
type CommonProps =
  ( setTermList  :: NgramsTerm -> Maybe TermList -> TermList -> Effect Unit
  , text         :: Maybe String
  , mode         :: ModeType
  , cache        :: Record Cache
  )
type AnnotatedFieldProps =
  ( -- ngrams       :: T.Box NgramsTable
  ngrams'  :: NgramsTable
  | CommonProps )
type MouseEvent = E.SyntheticEvent DE.MouseEvent

-- UNUSED
-- defaultProps :: Record Props
-- defaultProps = { ngrams: NgramsTable Map.empty, text: Nothing, setTermList: \_ _ _ -> pure unit }


annotatedField :: R2.Leaf AnnotatedFieldProps
annotatedField = R2.leaf annotatedFieldCpt
annotatedFieldCpt :: R.Component AnnotatedFieldProps
annotatedFieldCpt = here.component "annotatedField" cpt where
  cpt props@{ ngrams' } _ = do
    -- ngrams' <- T.useLive T.unequal props.ngrams

    menuRef <- R.useRef (Nothing :: Maybe (Record AnnotationMenu))
    redrawMenu <- T.useBox false
    reload <- T.useBox T2.newReload

    let commonProps = RX.pick props :: Record CommonProps

    -- R.useEffect' $ do
    --   let NgramsTable { ngrams_repo_elements: nre } = ngrams'
    --   here.log2 "[annotatedField] ngrams' keys" $ A.fromFoldable $ Map.keys nre

    pure $ annotatedFieldInner (Record.merge { menuRef, ngrams: ngrams', redrawMenu, reload } commonProps)

-----------------------------------------------------------------

type InnerProps =
  ( ngrams     :: NgramsTable
  , menuRef    :: R.Ref (Maybe (Record AnnotationMenu))
  , redrawMenu :: T.Box Boolean
  , reload     :: T2.ReloadS
  | CommonProps
  )

annotatedFieldInner :: R2.Leaf InnerProps
annotatedFieldInner = R2.leaf annotatedFieldInnerCpt
annotatedFieldInnerCpt :: R.Component InnerProps
annotatedFieldInnerCpt = here.component "annotatedFieldInner" cpt where
  cpt { menuRef
      , ngrams
      , redrawMenu
      , setTermList
      , text: fieldText
      , mode
      , cache
      , reload
      } _ = do
    -- | States
    -- |
    _redrawMenu' <- T.useLive T.unequal redrawMenu

    -- menu <- T.useBox (Nothing :: Maybe (Record AnnotationMenu))

    -- | Computed
    -- |
    let
      wrap :: Tuple String (List (Tuple NgramsTerm TermList)) -> Record RunProps
      wrap (text /\ list)
        = { list
          , onSelect: onAnnotationSelect { menuRef, ngrams, redrawMenu, setTermList, reload}
          , text
          }

    -- | Render
    -- |
    pure $

      H.div
      { className: "annotated-field-wrapper" }
      [
        annotationMenu { menuRef }
      ,
        case mode of

          EditionMode ->

            H.div
            { className: "annotated-field-runs"
            , aria: { expanded: false }
            }
            ((\p -> annotateRun p) <$> wrap <$> compile cache ngrams fieldText)


          AdditionMode ->

            R2.fromMaybe fieldText \t ->

              H.div
              { className: "annotated-field-runs" }
              [
                annotateRun
                { list: mempty
                , text: t
                , onSelect: onAnnotationSelect { menuRef, ngrams, redrawMenu, setTermList, reload }
                }
              ]
      ]

-----------------------------------------------------------

compile ::
     Record Cache
  -> NgramsTable
  -> Maybe String
  -> Array HighlightElement
compile cache ngrams = maybe [] (highlightNgrams cache CTabTerms ngrams)

-- Runs

onAnnotationSelect :: forall e.
     DE.IsMouseEvent e
  => { menuRef      :: R.Ref (Maybe (Record AnnotationMenu))
     , ngrams       :: NgramsTable
     , redrawMenu   :: T.Box Boolean
     , setTermList  :: NgramsTerm -> Maybe TermList -> TermList -> Effect Unit
     , reload       :: T2.ReloadS
     }
  -> Maybe (Tuple NgramsTerm TermList)
  -> E.SyntheticEvent e
  -> Effect Unit
onAnnotationSelect
  { menuRef, ngrams, redrawMenu, setTermList, reload}
  Nothing
  event
    = do
    s <- Sel.getSelection
    case s of
      Just sel -> do
        case (normNgram CTabTerms $ Sel.selectionToString sel) of
          NormNgramsTerm "" -> hideMenu { menuRef, redrawMenu }
          sel' -> do
            showMenu { event
                     , getList: findNgramTermList ngrams
                     , menuRef
                     , menuType: NewNgram
                     , ngram: sel'  -- normNgram CTabTerms sel'
                     , redrawMenu
                     , setTermList
                     , reload }
      Nothing -> hideMenu { menuRef, redrawMenu }

onAnnotationSelect
  { menuRef, redrawMenu, setTermList, reload }
  (Just (Tuple ngram list))
  event
    = showMenu
      { event
      , getList: const (Just list)
      , menuRef
      , menuType: SetTermListItem
      , ngram
      , redrawMenu
      , setTermList
      , reload
      }

-- showMenu :: forall p e. DE.IsMouseEvent e => { event :: E.SyntheticEvent e | p } -> Effect Unit
showMenu :: forall e.
     DE.IsMouseEvent e
  => { event       :: E.SyntheticEvent e
     , getList     :: NgramsTerm -> Maybe TermList
     , menuRef     :: R.Ref (Maybe (Record AnnotationMenu))
     , menuType    :: MenuType
     , ngram       :: NgramsTerm
     , redrawMenu  :: T.Box Boolean
     , setTermList :: NgramsTerm -> Maybe TermList -> TermList -> Effect Unit
     , reload      :: T2.ReloadS
     }
  -> Effect Unit
showMenu
  { event, getList, menuRef, menuType, ngram, redrawMenu, setTermList, reload }
    = do
    let x = E.clientX event
        y = E.clientY event
        -- n = normNgram CTabTerms text
        list = getList ngram
        -- redrawMenu = T.modify not redrawMenu
        setList t = do
          setTermList ngram list t
          hideMenu { menuRef, redrawMenu }
          T2.reload reload
    E.preventDefault event
    --range <- Sel.getRange sel 0
    --here.log2 "selection range" $ Sel.rangeToTuple range
    let menu = Just
          { list
          , menuType
          , closeCallback: const $ hideMenu { menuRef, redrawMenu }
          , redrawMenu
          , setList
          , x
          , y }
    R.setRef menuRef menu
    T.modify_ not redrawMenu

hideMenu ::
     { menuRef     :: R.Ref (Maybe (Record AnnotationMenu))
     , redrawMenu  :: T.Box Boolean
     }
  -> Effect Unit
hideMenu { menuRef, redrawMenu } = do
  R.setRef menuRef Nothing
  T.modify_ not redrawMenu

--------------------------------------------------

type RunProps =
  ( list       :: List (Tuple NgramsTerm TermList)
  , onSelect   :: Maybe (Tuple NgramsTerm TermList) -> MouseEvent -> Effect Unit
  , text       :: String
  )

annotateRun :: R2.Leaf RunProps
annotateRun = R2.leaf annotatedRunCpt
annotatedRunCpt :: R.Component RunProps
annotatedRunCpt = here.component "annotatedRun" cpt where
  cpt { list, onSelect, text } _ = pure $ case list of

    Nil ->
        H.span
        { on: { mouseUp: onSelect Nothing }
        }
        [ H.text text ]

    lst@(( ngram /\ list' ) : _) ->
      let
        bgClasses
            = joinWith " " $ A.fromFoldable $ termClass
          <<< snd <$> lst

      in
        H.span
        { className: "annotation-run " <> bgClasses
        , on: { click: onSelect (Just (ngram /\ list')) }
        }
        [ H.text text ]
