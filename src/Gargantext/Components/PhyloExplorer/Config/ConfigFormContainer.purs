module Gargantext.Components.PhyloExplorer.ConfigFormParser
  ( useConfigFormParser
  ) where

import Gargantext.Prelude

import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Number as Number
import Gargantext.Components.PhyloExplorer.API (Clique(..), CliqueFilter, ReflexiveClique(..), ReflexiveTimeUnit, TimeUnitCriteria(..), UpdateData(..), extractCriteria, fromReflexiveTimeUnit, toReflexiveTimeUnit)
import Gargantext.Components.PhyloExplorer.ConfigForm (FormData)
import Gargantext.Types (FrontendError(..))
import Gargantext.Utils (getter)
import Reactix as R
import Record (merge)
import Unsafe.Coerce (unsafeCoerce)

type Methods =
  -- | Parse `PhyloExplorer.API.UpdateData` to hydrate optional properties of
  -- | `ConfigForm.FormData`
  -- |
  -- | (!) I/O as `UpdateData` type can be changed to anything, it has been
  -- |     chosen this way for simplification (KISS choice: API ⟷ FormData)
  ( toFormData    :: UpdateData      -> Record ()
  -- | Parse callback returned data from `ConfigForm.FormData` into the
  -- | `PhyloExplorer.API.UpdateData`
  , fromFormData  :: Record FormData -> Either FrontendError UpdateData
  )

useConfigFormParser :: R.Hooks (Record Methods)
useConfigFormParser = do

  let
    castError ::
         Either String        UpdateData
      -> Either FrontendError UpdateData
    castError (Left error) = Left $ FOtherError { error }
    castError (Right a)    = pure a

  pure
    { toFormData
    , fromFormData: (_ # fromFormData) >>> castError
    }


toFormData :: UpdateData -> Record ()
toFormData nt =
  let r = unwrap nt
  in unsafeCoerce $
    { proximity:
        show r.proximity
    , synchrony:
        show r.synchrony
    , quality:
        show r.quality
    , exportFilter:
        show r.exportFilter
    -- Time unit
    , granularity: r #
        (show <<< toReflexiveTimeUnit <<< _.timeUnit)
    , period: r #
        (show <<< getter _.period <<< extractCriteria <<< _.timeUnit)
    , step: r #
        (show <<< getter _.step <<< extractCriteria <<< _.timeUnit)
    , matchingFrame: r #
        (show <<< getter _.matchingFrame <<< extractCriteria <<< _.timeUnit)
    -- Clique
    } `merge` parseClique r.clique
  where
    parseClique :: Clique -> Record ()
    parseClique (FIS o) = unsafeCoerce $
      { support     : show o.support
      , size        : show o.size
      , cliqueType  : show FIS_
      }
    parseClique (MaxClique o) = unsafeCoerce $
      { size        : show o.size
      , threshold   : show o.threshold
      , cliqueFilter: show o.filter
      , cliqueType  : show MaxClique_
      }

fromFormData :: Record FormData -> Either String UpdateData
fromFormData r = do
  -- Common params
  proximity <-
    Number.fromString r.proximity `orDie` "Invalid proximity"
  synchrony <-
    Number.fromString r.synchrony `orDie` "Invalid synchrony"
  quality <-
    Number.fromString r.quality `orDie` "Invalid quality"
  exportFilter <-
    Number.fromString r.exportFilter `orDie` "Invalid exportFilter"

  -- Time unit params
  (granularity :: ReflexiveTimeUnit) <-
    read r.granularity `orDie` "Invalid granularity"
  period <-
    Int.fromString r.period `orDie` "Invalid period"
  step <-
    Int.fromString r.step `orDie` "Invalid step"
  matchingFrame <-
    Int.fromString r.matchingFrame `orDie` "Invalid matchingFrame"

  criteria <- pure $
    parseCriteria period step matchingFrame
  timeUnit <- pure $
    fromReflexiveTimeUnit granularity criteria

  -- Clique params
  (cliqueType :: ReflexiveClique) <-
    read r.cliqueType `orDie` "Invalid cliqueType"

  clique <-
    parseClique r cliqueType

  -- Constructor
  pure $ UpdateData
    { proximity
    , synchrony
    , quality
    , exportFilter
    , timeUnit
    , clique
    }
  where
    parseCriteria :: Int -> Int -> Int -> TimeUnitCriteria
    parseCriteria period step matchingFrame = TimeUnitCriteria
      { period
      , step
      , matchingFrame
      }

    parseClique ::
         Record FormData
      -> ReflexiveClique
      -> Either String Clique
    parseClique o = case _ of
      FIS_ -> ado
        support <-
          Int.fromString o.support `orDie` "Invalid support"
        size <-
          Int.fromString o.size `orDie` "Invalid size"
        in
          FIS { support, size }
      MaxClique_ -> ado
        size <-
          Int.fromString o.size `orDie` "Invalid size"
        threshold <-
          Number.fromString o.threshold `orDie` "Invalid threshold"
        (filter :: CliqueFilter) <-
          read o.cliqueFilter `orDie` "Invalid cliqueFilter"
        in
          MaxClique { size, threshold, filter }

orDie :: forall err a. Maybe a -> err -> Either err a
orDie (Just a) _   = pure a
orDie Nothing  err = Left err
