module Gargantext.Utils.Reactix where

import Prelude

import DOM.Simple as DOM
import DOM.Simple.Console (log, log2)
import DOM.Simple.Document (document)
import DOM.Simple.Element as Element
import DOM.Simple.Event as DE
import DOM.Simple.Types (class IsNode)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, null, toMaybe)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, launchAff, launchAff_, killFiber)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Uncurried (EffectFn1, runEffectFn1, mkEffectFn1, mkEffectFn2)
import FFI.Simple ((...), defineProperty, delay, args2, args3)
import React (class ReactPropFields, Children, ReactClass, ReactElement)
import React as React
import Reactix as R
import Reactix.DOM.HTML (ElemFactory, createDOM, text)
import Reactix.React (react)
import Reactix.SyntheticEvent as RE
import Reactix.Utils (currySecond, hook, tuple)
import Unsafe.Coerce (unsafeCoerce)

newtype Point = Point { x :: Number, y :: Number }

-- a setter function, for useState
type Setter t = (t -> t) -> Effect Unit
-- a reducer function living in effector, for useReductor
type Actor s a = (a -> s -> Effect s)

-- | Turns a ReactElement into aReactix Element
-- | buff (v.) to polish
buff :: ReactElement -> R.Element
buff = unsafeCoerce

-- | Turns a Reactix Element into a ReactElement.
-- | scuff (v.) to spoil the gloss or finish of.
scuff :: R.Element -> ReactElement
scuff = unsafeCoerce

-- class ToElement a where
--   toElement :: a -> R.Element

-- instance toElementElement :: ToElement R.Element where
--   toElement = identity

-- instance toElementReactElement :: ToElement ReactElement where
--   toElement = buff

-- instance toElementArray :: ToElement a => ToElement (Array a) where
--   toElement = R.fragment <<< map toElement

createElement' :: forall required given
                . ReactPropFields required given
               => ReactClass { children :: Children | required }
               -> Record given -> Array R.Element -> R.Element
createElement' reactClass props children =
  buff $ React.createElement reactClass props $ scuff <$> children

{-
instance isComponentReactClass
      :: R.IsComponent (ReactClass { children :: Children
                                   | props
                                   }) props (Array R.Element) where
  createElement reactClass props children =
    React.createElement reactClass props children
-}

-- | Turns an aff into a useEffect-compatible Effect (Effect Unit)
affEffect :: forall a. String -> Aff a -> Effect (Effect Unit)
affEffect errmsg aff = do
    fiber <- launchAff aff
    pure $ launchAff_ $ killFiber (error errmsg) fiber

mousePosition :: RE.SyntheticEvent DE.MouseEvent -> Point
mousePosition e = Point { x: RE.clientX e, y: RE.clientY e }

domMousePosition :: DE.MouseEvent -> Point
domMousePosition = mousePosition <<< unsafeCoerce
-- | This is naughty, it quietly mutates the input and returns it
named :: forall o. String -> o -> o
named = flip $ defineProperty "name"

overState :: forall t. (t -> t) -> R.State t -> Effect Unit
overState f (_state /\ setState) = setState f

select :: ElemFactory
select = createDOM "select"

menu :: ElemFactory
menu = createDOM "menu"

effToggler :: forall e. R.State Boolean -> EffectFn1 e Unit
effToggler (value /\ setValue) = mkEffectFn1 $ \e -> setValue $ const $ not value

unsafeEventValue :: forall event. event -> String
unsafeEventValue e = (unsafeCoerce e).target.value

nullRef :: forall t. R.Hooks (R.Ref (Nullable t))
nullRef = R.useRef null

nothingRef :: forall t. R.Hooks (R.Ref (Maybe t))
nothingRef = R.useRef Nothing

useLayoutEffect1' :: forall a. a -> (Unit -> Effect Unit) -> R.Hooks Unit
useLayoutEffect1' a f = R.useLayoutEffect1 a $ do
  liftEffect $ f unit
  pure $ pure unit

useLayoutRef :: forall a b. (a -> b) -> b -> R.Ref a -> R.Hooks (R.Ref b)
useLayoutRef fn init ref = do
  new <- R.useRef init
  let old = R.readRef ref
  useLayoutEffect1' old $ \_ -> R.setRef new (fn old)
  pure new

usePositionRef :: R.Ref (Nullable DOM.Element) -> R.Hooks (R.Ref (Maybe DOM.DOMRect))
usePositionRef = useLayoutRef (map Element.boundingRect <<< toMaybe) Nothing

readPositionRef :: R.Ref (Nullable DOM.Element) -> Maybe DOM.DOMRect
readPositionRef el = do
  let posRef = R.readRef el
  Element.boundingRect <$> toMaybe posRef

unsafeEventTarget :: forall event. event -> DOM.Element
unsafeEventTarget e = (unsafeCoerce e).target

getElementById :: String -> Effect (Maybe DOM.Element)
getElementById = (flip delay) h
  where
    h id = pure $ toMaybe $ document ... "getElementById" $ [id]

-- We just assume it works, so make sure it's in the html
getPortalHost :: R.Hooks DOM.Element
getPortalHost = R.unsafeHooksEffect $ delay unit $ \_ -> pure $ document ... "getElementById" $ ["portal"]

useLayoutEffectOnce :: Effect (Effect Unit) -> R.Hooks Unit
useLayoutEffectOnce e = R.unsafeUseLayoutEffect e []

singleParent :: forall props. R.Component props -> Record props -> R.Element -> R.Element
singleParent cpt props child = R.createElement cpt props [ child ]

childless :: forall props. R.Component props -> Record props -> R.Element
childless cpt props = R.createElement cpt props []

showText :: forall s. Show s => s -> R.Element
showText = text <<< show

----- Reactix's new effectful reducer: sneak-peek because anoe wants to demo on tuesday

-- | Like a reducer, but lives in Effect
type Reductor state action = Tuple state (action -> Effect Unit)

-- | Like useReductor, but lives in Effect
useReductor :: forall s a i. Actor s a -> (i -> Effect s) -> i -> R.Hooks (Reductor s a)
useReductor f i j =
  hook $ \_ ->
    pure $ currySecond $ tuple $ react ... "useReducer" $ args3 (mkEffectFn2 (flip f)) j (mkEffectFn1 i)

-- | Like `useReductor`, but takes an initial state instead of an
-- | initialiser function and argument
useReductor' :: forall s a. Actor s a -> s -> R.Hooks (Reductor s a)
useReductor' r = useReductor r pure

render :: R.Element -> DOM.Element -> Effect Unit
render e d = delay unit $ \_ -> pure $ R.reactDOM ... "render" $ args2 e d

addRootElement :: DOM.Element -> Effect Unit
addRootElement = runEffectFn1 _addRootElement

foreign import _addRootElement
  :: EffectFn1 DOM.Element Unit

appendChild :: forall n m. IsNode n => IsNode m => n -> m -> Effect Unit
appendChild n c = delay unit $ \_ -> pure $ n ... "appendChild" $ [c]

appendChildToParentId :: forall c. IsNode c => String -> c -> Effect Unit
appendChildToParentId ps c = delay unit $ \_ -> do
  parentEl <- getElementById ps
  log2 "[appendChildToParentId] ps" ps
  log2 "[appendChildToParentId] parentEl" parentEl
  case parentEl of
    Nothing -> pure unit
    Just el -> appendChild el c


useCache :: forall i o. Eq i => i -> (i -> R.Hooks o) -> R.Hooks o
useCache i f = do
  iRef <- R.useRef Nothing
  oRef <- R.useRef Nothing
  let currI = R.readRef iRef
  let currO = R.readRef oRef
  if currI == Just i then
    case currO of
      Nothing -> f i -- this one shouldn't happen, but purescript
      Just v -> pure v
  else do
    new <- f i
    R.unsafeHooksEffect (R.setRef oRef $ Just new)
    R.unsafeHooksEffect (R.setRef iRef $ Just i)
    pure new
