module Gargantext.Utils.Console
  ( Console, RowConsole
  , encloseContext
  , CalleeType(..)
  , LogType(..)
  , print, print2, print3
  ) where

import Gargantext.Prelude

import DOM.Simple.Console (log5, log6, log7)
import Data.Array (unsafeIndex)
import Data.String (Pattern(..), Replacement(..), replace)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)

foreign import getCreationDate  :: Effect String -- @TODO not using FFI

type CalleeName = String

data CalleeType
  = Main

data LogType
  = Log
  | Error
  | Warn
  | Info

derive instance eqLogType :: Eq LogType

type RowConsole =
  ( log     :: forall a. a -> Effect Unit
  , error   :: forall a. a -> Effect Unit
  , warn    :: forall a. a -> Effect Unit
  , info    :: forall a. a -> Effect Unit
  , log2    :: forall a b. a -> b -> Effect Unit
  , error2  :: forall a b. a -> b -> Effect Unit
  , warn2   :: forall a b. a -> b -> Effect Unit
  , info2   :: forall a b. a -> b -> Effect Unit
  , log3    :: forall a b c. a -> b -> c -> Effect Unit
  , error3  :: forall a b c. a -> b -> c -> Effect Unit
  , warn3   :: forall a b c. a -> b -> c -> Effect Unit
  , info3   :: forall a b c. a -> b -> c -> Effect Unit
  )

type Console = Record RowConsole

-- | Logging as JavaScript fancy way
-- |
-- | Enclose the appling context
-- | ```purescript
-- |  console :: Console
-- |  console = encloseContext "page" "reactix"
-- | ```
-- |
-- | And reuse it as a native JavaScript call
-- | ```purescript
-- |  console.log2 "catch something" somethingRecord
-- | ```
encloseContext :: CalleeType -> CalleeName -> Console
encloseContext a b =
  { log    : print  a b Log
  , error  : print  a b Error
  , warn   : print  a b Warn
  , info   : print  a b Info
  , log2   : print2 a b Log
  , error2 : print2 a b Error
  , warn2  : print2 a b Warn
  , info2  : print2 a b Info
  , log3   : print3 a b Log
  , error3 : print3 a b Error
  , warn3  : print3 a b Warn
  , info3  : print3 a b Info
  }

bulletContent :: LogType -> String
bulletContent Log = "▷"
bulletContent _   = "▶"

bulletStyle :: LogType -> String
bulletStyle s = replace (Pattern "%s") (Replacement s') bulletCSS
  where
    s' = case s of
      Error -> "#BF3F3F"
      Warn  -> "#D4CC5B"
      Info  -> "#69A1F0"
      Log   -> "#A9A9A9"

calleeStyle :: CalleeType -> String
calleeStyle s = replace (Pattern "%s") (Replacement s') calleeCSS
  where
    s' = case s of
      Main      -> "#E9ECEF"

print :: forall a. CalleeType -> CalleeName -> LogType -> a -> Effect Unit
print s s' s'' a = do
  cells <- mkCells s s' s''
  log5 (unsafePartial $ unsafeIndex cells 0)
       (unsafePartial $ unsafeIndex cells 1)
       (unsafePartial $ unsafeIndex cells 2)
       (unsafePartial $ unsafeIndex cells 3)
       a

print2 :: forall a b. CalleeType -> CalleeName -> LogType -> a -> b -> Effect Unit
print2 s s' s'' a b = do
  cells <- mkCells s s' s''
  log6 (unsafePartial $ unsafeIndex cells 0)
       (unsafePartial $ unsafeIndex cells 1)
       (unsafePartial $ unsafeIndex cells 2)
       (unsafePartial $ unsafeIndex cells 3)
       a b

print3 :: forall a b c. CalleeType -> CalleeName -> LogType -> a -> b -> c -> Effect Unit
print3 s s' s'' a b c = do
  cells <- mkCells s s' s''
  log7 (unsafePartial $ unsafeIndex cells 0)
       (unsafePartial $ unsafeIndex cells 1)
       (unsafePartial $ unsafeIndex cells 2)
       (unsafePartial $ unsafeIndex cells 3)
       a b c

mkCells :: CalleeType -> CalleeName -> LogType -> Effect (Array String)
mkCells calleeType calleeName logType = do
  date <- getCreationDate
  -- First cell: containing a concatenation of string with style signs "%c"
  cell1 <- pure $ ("%c" <> bulletContent logType <> "%c" <> date <> "%c" <> calleeName)
  -- Second cell: providing style for the first "%c"
  cell2 <- pure $ bulletStyle $ logType
  -- Third cell: idem
  cell3 <- pure $ creationDateCSS
  -- Fourth cell: idem
  cell4 <- pure $ calleeStyle calleeType
  pure $ [ cell1, cell2, cell3, cell4 ]
  -- Next cells can either be in couple (content + style) or content one only

--------------------------------------------------------------------

bulletCSS :: String
bulletCSS = "color: %s; padding: 4px 5px; font-weight: bold;"

calleeCSS :: String
calleeCSS = "color: #495057; background: %s; padding: 4px 5px; font-size: 10px;"

creationDateCSS :: String
creationDateCSS = "font-size: 9px; padding: 4px 5px;"
