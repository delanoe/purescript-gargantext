-- |
-- The present module has been ported from Haskell to PureScript
-- by Nicolas Pouillard for the Gargantext projet.
--
-- Original Haskell code:
--   Copyright      : (c) 2010 Daniel Fischer
--   Licence        : BSD3
--   Maintainer     : Daniel Fischer <daniel.is.fischer@googlemail.com>
--
-- Simultaneous search for multiple patterns in a 'String'
-- using the Karp-Rabin algorithm.
--
-- A description of the algorithm for a single pattern can be found at
-- <http://www-igm.univ-mlv.fr/~lecroq/string/node5.html#SECTION0050>.
module Gargantext.Utils.KarpRabin ( -- * Overview
                                    -- $overview

                                    -- ** Caution
                                    -- $caution

                                    -- * Function
                                    indicesOfAny
                                  ) where


import Data.Array as A
import Data.Enum (fromEnum)
import Data.Foldable (class Foldable, minimum, foldl)
import Data.Int (quot)
import Data.List as L
import Data.List (List)
import Data.Map as M
import Data.Maybe (Maybe(..), isJust)
import Data.String as S
import Data.String (CodePoint)
import Data.Tuple (Tuple(..))
import Data.UInt (UInt, shl, fromInt)
import Partial.Unsafe (unsafePartial)

import Prelude

fromCodePoint :: CodePoint -> UInt
fromCodePoint c = fromInt (fromEnum c)

-- $overview
--
-- The Karp-Rabin algorithm works by calculating a hash of the pattern and
-- comparing that hash with the hash of a slice of the target string with
-- the same length as the pattern. If the hashes are equal, the slice of the
-- target is compared to the pattern byte for byte (since the hash
-- function generally isn't injective).
--
-- For a single pattern, this tends to be more efficient than the na&#239;ve
-- algorithm, but it cannot compete with algorithms like
-- Knuth-Morris-Pratt or Boyer-Moore.
--
-- However, the algorithm can be generalised to search for multiple patterns
-- simultaneously. If the shortest pattern has length @k@, hash the prefix of
-- length @k@ of all patterns and compare the hash of the target's slices of
-- length @k@ to them. If there's a match, check whether the slice is part
-- of an occurrence of the corresponding pattern.
--
-- With a hash-function that
--
--   * allows to compute the hash of one slice in constant time from the hash
--     of the previous slice, the new and the dropped character, and
--
--   * produces few spurious matches,
--
-- searching for occurrences of any of @n@ patterns has a best-case complexity
-- of /O/(@targetLength@ * @lookup n@). The worst-case complexity is
-- /O/(@targetLength@ * @lookup n@ * @sum patternLengths@), the average is
-- not much worse than the best case.
--
-- The functions in this module store the hashes of the patterns in an
-- 'Map', so the lookup is /O/(@log n@). Re-hashing is done in constant
-- time and spurious matches of the hashes /should be/ sufficiently rare.
-- The maximal length of the prefixes to be hashed is 32.

-- $caution
--
-- Unfortunately, the constant factors are high, so these functions are slow.
-- Unless the number of patterns to search for is high (larger than 50 at
-- least), repeated search for single patterns using Boyer-Moore or DFA and
-- manual merging of the indices is faster. /Much/ faster for less than 40
-- or so patterns.
--
-- In summary, this module is more of an interesting curiosity than anything
-- else.

-- | @'indicesOfAny'@ finds all occurrences of any of several non-empty patterns
--   in a strict target string. If no non-empty patterns are given,
--   the result is an empty array. Otherwise the result array contains
--   the pairs of all indices where any of the (non-empty) patterns start
--   and the array of all patterns starting at that index, the patterns being
--   represented by their (zero-based) position in the pattern array.
--   Empty patterns are filtered out before processing begins.
indicesOfAny :: Array String                  -- ^ Array of non-empty patterns
             -> String                        -- ^ String to search
             -> Array (Tuple Int (Array Int)) -- ^ Array of matches
indicesOfAny pats = if A.null nepats then const []
                                     else strictMatcher nepats
      where
        nepats = A.filter (not <<< S.null) pats


------------------------------------------------------------------------------
--                                 Workers                                 --
------------------------------------------------------------------------------

rehash' :: UInt -> UInt -> UInt -> CodePoint -> CodePoint -> UInt
rehash' shDi out h o n =
    (h `shl` shDi - (fromCodePoint o `shl` out)) + fromCodePoint n

minimum1 :: forall a f. Ord a => Foldable f => a -> f a -> a
minimum1 a fa =
  case minimum fa of
    Nothing -> a
    Just b  -> min a b

strictMatcher :: Array String -> String -> Array (Tuple Int (Array Int))
strictMatcher pats = unsafePartial search
  where
    hLen = minimum1 32 (S.length <$> pats)
    hLen' = fromInt hLen
    shDi = case 32 `quot` hLen of
              q | q < 4 -> q
                | otherwise -> 4
    outS = fromInt (shDi * hLen)
    patNum = A.length pats
    rehash :: UInt -> CodePoint -> CodePoint -> UInt
    rehash = case shDi of
                1 -> rehash' (fromInt 1) hLen'
                2 -> rehash' (fromInt 2) outS
                3 -> rehash' (fromInt 3) outS
                _ -> rehash' (fromInt 4) outS
    hash :: String -> UInt
    hash = foldl (\h w -> (h `shl` fromInt shDi) + fromCodePoint w) (fromInt 0)
        <<< S.toCodePointArray
        <<< S.take hLen
    hashMap =
      M.fromFoldableWith (flip (<>))
                         (A.mapWithIndex (\i a -> Tuple (hash a) [i]) pats)
    search :: Partial => String -> Array (Tuple Int (Array Int))
    search str = if strLen < hLen then []
                                  else A.fromFoldable (go 0 shash)
          where
            strLen = S.length str
            maxIdx = strLen - hLen
            arr = S.toCodePointArray str
            strAt i = A.unsafeIndex arr i
            shash = hash str
            go sI h =
              case M.lookup h hashMap of
                Nothing ->
                  if sI == maxIdx
                    then L.Nil
                    else go (sI + 1) (rehash h (strAt sI) (strAt (sI + hLen)))
                Just ps ->
                  let rst = S.drop sI str
                      hd  = strAt sI
                      more = if sI == maxIdx then L.Nil else
                                go (sI + 1) (rehash h hd (strAt (sI + hLen)))
                      okay bs =
                        isJust (S.stripPrefix (S.Pattern bs) rst)
                  in case A.filter (\x -> okay (A.unsafeIndex pats x)) ps of
                           [] -> more
                           qs -> Tuple sI qs L.: more
