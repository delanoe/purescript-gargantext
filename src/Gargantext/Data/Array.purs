module Gargantext.Data.Array where

import Control.Monad.Aff.Console (log)
import Data.Array

--
---- | The 'intersperse' function takes an element and a list and
---- \`intersperses\' that element between the elements of the list.
---- For example,
----
---- >>> intersperse ',' "abcde"
---- "a,b,c,d,e"
--intersperse             :: a -> [a] -> [a]
--intersperse _   []      = []
--intersperse sep (x:xs)  = x : prependToAll sep xs
--
--
---- We want to make every element in the 'intersperse'd list available
---- as soon as possible to avoid space leaks. Experiments suggested that
---- a separate top-level helper is more efficient than a local worker.
--prependToAll            :: a -> [a] -> [a]
--prependToAll _   []     = []
--prependToAll sep (x:xs) = sep : x : prependToAll sep xs
--
---- | 'intercalate' @xs xss@ is equivalent to @('concat' ('intersperse' xs xss))@.
---- It inserts the list @xs@ in between the lists in @xss@ and concatenates the
---- result.
----
---- >>> intercalate ", " ["Lorem", "ipsum", "dolor"]
---- "Lorem, ipsum, dolor"
--intercalate :: [a] -> [[a]] -> [a]
--intercalate xs xss = concat (intersperse xs xss)
--

