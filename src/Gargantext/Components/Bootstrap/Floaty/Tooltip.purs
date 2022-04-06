module Gargantext.Components.Bootstrap.Tooltip
  ( tooltip
  , tooltipBind
  , tooltipContainer
  ) where

import Gargantext.Prelude

import ConvertableOptions as CO
import Data.Symbol (SProxy(..))
import Data.UUID as UUID
import Gargantext.Components.Bootstrap.Types (TooltipEffect(..), TooltipPosition(..), Variant(..))
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Record as Record
import Record.Extra as RX

foreign import reactTooltipCpt :: R.Component Props

type Props =
  ( id          :: String
  | Options
  )

type Options =
  ( effect      :: TooltipEffect
  , variant     :: Variant
  , delayHide   :: Int
  , delayShow   :: Int
  , className   :: String
  , position    :: TooltipPosition
  )

options :: Record Options
options =
  { effect      : SolidEffect
  , variant     : Dark
  , delayHide   : 0
  , delayShow   : 0
  , className   : ""
  , position    : AutomaticPosition
  }


-- | Adapter Component for React Tooltip
-- |
-- |
-- | @XXX: tooltip position not working
-- | @link https://github.com/wwayne/react-tooltip/issues/747
-- |
-- |
-- | https://github.com/wwayne/react-tooltip
tooltip :: forall provided.
     CO.Defaults (Record Options) (Record provided) (Record Props)
  => Record provided
  -> Array R.Element
  -> R.Element
tooltip props = R.rawCreateElement reactTooltipCpt props''
  where
    props'  = CO.defaults options props
    props'' = props'
        # Record.set
          (SProxy :: SProxy "effect")
          (show props'.effect)
      >>> Record.set
          (SProxy :: SProxy "variant")
          (show props'.variant)
      >>> Record.rename
          (SProxy :: SProxy "variant")
          (SProxy :: SProxy "type")
      >>> Record.set
          (SProxy :: SProxy "position")
          (show props'.position)
      >>> Record.rename
          (SProxy :: SProxy "position")
          (SProxy :: SProxy "place")

-------------------------------------------------------------

type TooltipBindingProps =
  ( "data-tip" :: Boolean
  , "data-for" :: String
  )

-- | Bind a Component props to an existing <tooltip>
tooltipBind :: String -> Record TooltipBindingProps
tooltipBind =
  { "data-for": _
  , "data-tip": true
  }

-------------------------------------------------------------

type ContainerProps =
  ( defaultSlot :: R.Element
  , tooltipSlot :: R.Element
  | Options
  )

tooltipContainer :: forall r. R2.OptLeaf Options ContainerProps r
tooltipContainer = R2.optLeaf tooltipContainerCpt options

tooltipContainerCpt :: R.Memo ContainerProps
tooltipContainerCpt = R.memo' $ R.hooksComponent "tooltipContainer" cpt where
  cpt props@{ tooltipSlot
            , defaultSlot
            } _
      = R.unsafeHooksEffect (UUID.genUUID >>= pure <<< UUID.toString)
    >>= \uuid -> do
      -- Computed
      let
        tooltipProps = Record.merge
          (RX.pick props :: Record Options)
          { id: uuid }

      pure $

        R2.fragmentWithKey uuid
        [
          tooltip
          tooltipProps
          [ tooltipSlot ]
        ,
          H.span
          (tooltipBind uuid)
          [ defaultSlot ]
        ]
