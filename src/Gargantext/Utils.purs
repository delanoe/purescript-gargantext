module Gargantext.Utils where

import Prelude

import Data.Char (fromCharCode)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldr)
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Sequence.Ordered as OSeq
import Data.Set (Set)
import Data.Set as Set
import Data.String as S
import Data.String.CodeUnits (singleton, slice)
import Data.Unfoldable (class Unfoldable)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Web.HTML as WHTML
import Web.HTML.Location as WHL
import Web.HTML.Window (location)

-- | TODO (hard coded)
csrfMiddlewareToken :: String
csrfMiddlewareToken = "Wy52D2nor8kC1r1Y4GrsrSIxQ2eqW8UwkdiQQshMoRwobzU4uldknRUhP0j4WcEM"

setterv :: forall nt record field.
           Newtype nt record
           => (record -> field -> record)
           -> field
           -> nt
           -> nt
setterv fn v t = (setter (flip fn v) t)

setter :: forall nt record.
          Newtype nt record
          => (record -> record)
          -> nt
          -> nt
setter fn = wrap <<< fn <<< unwrap

getter :: forall record field nt.
          Newtype nt record
          => (record -> field)
          -> nt
          -> field
getter fn = fn <<< unwrap

-- TODO: not optimal but Data.Set lacks some function (Set.alter)
toggleSet :: forall a. Ord a => a -> Set a -> Set a
toggleSet a s
  | Set.member a s = Set.delete a s
  | otherwise      = Set.insert a s

-- Default sort order is ascending, we may want descending
invertOrdering :: Ordering -> Ordering
invertOrdering LT = GT
invertOrdering GT = LT
invertOrdering EQ = EQ

-- A lens that always returns unit
_unit :: forall s. Lens' s Unit
_unit = lens (\_ -> unit) (\s _ -> s)

-- | Format a number with specified amount of zero-padding
zeroPad :: Int -> Int -> String
zeroPad pad num = zeros <> (show num)
  where
    numDigits = S.length $ show num
    zeros = if numDigits < pad then zeros' (pad - numDigits) else ""
    zeros' 0 = ""
    zeros' n = "0" <> (zeros' (n - 1))

queryNormalize :: String -> String
queryNormalize = S.toLower

queryMatchesLabel :: String -> String -> Boolean
queryMatchesLabel q l = S.contains (S.Pattern $ queryNormalize q) (queryNormalize l)

queryExactMatchesLabel :: String -> String -> Boolean
queryExactMatchesLabel q l = queryNormalize q == queryNormalize l


mapLeft :: forall l m r. (l -> m) -> Either l r -> Either m r
mapLeft f (Left  l) = Left (f l)
mapLeft _ (Right r) = Right r

data On a b = On a b

instance Eq a => Eq (On a b) where
  eq (On x _) (On y _) = eq x y

instance Ord a => Ord (On a b) where
  compare (On x _) (On y _) = compare x y

-- same as
-- https://github.com/purescript/purescript-arrays/blob/v5.3.1/src/Data/Array.purs#L715-L715
sortWith :: forall a b f. Functor f =>
                          Foldable f =>
                          Unfoldable f =>
                          Ord b =>
                          (a -> b) -> f a -> f a
sortWith f = map (\(On _ y) -> y) <<< OSeq.toUnfoldable <<< foldr (\x -> OSeq.insert (On (f x) x)) OSeq.empty


href :: Effect String
href = do
  w <- WHTML.window
  loc <- location w
  WHL.href loc


nbsp :: Int -> String
nbsp = nbsp' ""
  where
    char = singleton $ unsafePartial $ fromJust $ fromCharCode 160
    nbsp' acc n
      | n <= 0 = acc
      | otherwise = nbsp' (acc <> char) (n - 1)

ifElse :: forall a. Boolean -> a -> a -> a
ifElse predicate a b = if predicate then a else b

infixl 1 ifElse as ?


textEllipsisBreak :: Int -> String -> String
textEllipsisBreak len n =
  if S.length n < len then n
  else case (slice 0 len n) of
    "" -> "???"
    s  -> s <> "…"
