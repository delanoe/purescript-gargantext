module Gargantext.Utils where

import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldr)
import Data.Lens (Lens', lens)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Set (Set)
import Data.Set as Set
import Data.Sequence.Ordered as OSeq
import Data.String as S
import Data.Unfoldable (class Unfoldable)
import DOM.Simple.Window (window)
import Effect (Effect)
import Prelude
import Web.HTML (window)
import Web.HTML.Window (location)
import Web.HTML.Location as WHL

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

glyphicon :: String -> String
glyphicon t = "btn glyphitem fa fa-" <> t

glyphiconActive :: String -> Boolean -> String
glyphiconActive icon b = glyphicon icon <> if b then " active" else ""

-- | Format a number with specified amount of zero-padding
zeroPad :: Int -> Int -> String
zeroPad pad num = zeros <> (show num)
  where
    numDigits = S.length $ show num
    zeros = if numDigits < pad then zeros' (pad - numDigits) else ""
    zeros' 0 = ""
    zeros' n = "0" <> (zeros' (n - 1))

queryMatchesLabel :: String -> String -> Boolean
queryMatchesLabel q l = S.contains (S.Pattern $ normalize q) (normalize l)
  where
    normalize = S.toLower


mapLeft :: forall l m r. (l -> m) -> Either l r -> Either m r
mapLeft f (Left  l) = Left (f l)
mapLeft _ (Right r) = Right r

data On a b = On a b

instance eqOn :: Eq a => Eq (On a b) where
  eq (On x _) (On y _) = eq x y

instance ordOn :: Ord a => Ord (On a b) where
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
  w <- window
  loc <- location w
  WHL.href loc
