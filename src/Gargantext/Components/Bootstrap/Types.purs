module Gargantext.Components.Bootstrap.Types
  ( ComponentStatus(..)
  , Variant(..), ButtonVariant(..)
  , Sizing(..)
  , SpinnerTheme(..)
  , TooltipEffect(..), TooltipPosition(..)
  , Position(..)
  , Elevation(..)
  , ModalSizing(..)
  ) where

import Gargantext.Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.Extra (kebabCase)

-- | Component status based on UI/UX overall expression
-- |
-- |    * `Enabled`: default UI/UX behavior
-- |    * `Disabled`: main action of the component has been deactivated, and
-- |      a UI feedback is showed to the user (eg. a disabled button is now
-- |      unclikable, fade color, and disabled CTA feature)
-- |    * `Deffered`: main action of the component has been deactivated, but
-- |      contrary to a disabled state, the altered UX/UI bears characteristics
-- |      of a short-lived state (eg. a button turns to `Deffered`, is now
-- |      unclickable but presents a less inoperative style, for example a
-- |      spinner is now attached in place of the CTA text)
-- |    * `Muted`: on surface the component seems functional, but main action
-- |      of the component has been deactivated, yet no UI nor UX feedback is
-- |      particularly showed to the user accordingly
-- |    * `Idled`: balance between a `Disabled` and `Muted` state, as if the
-- |      component has its main feature deactivated, but told with a less
-- |      strong UI/UX (eg. a input in a "read-only" mode: UI can be alter to
-- |      underline the lack of its main writing feature, but without telling
-- |      to the user that the input is per-se inoperative)
data ComponentStatus
  = Enabled
  | Disabled
  | Deferred
  | Idled
  | Muted

derive instance Generic ComponentStatus _
derive instance Eq ComponentStatus
instance Show ComponentStatus where
  show = kebabCase <<< genericShow

----------------------------------------------------------------------

-- | Common variant style used by various Bootstrap components
-- |
-- |    * some component will combine different variant style
data Variant
  = Primary
  | Secondary
  | Success
  | Danger
  | Warning
  | Info
  | Light
  | Dark

derive instance Generic Variant _
derive instance Eq Variant
instance Show Variant where
  show = kebabCase <<< genericShow

-- Component declinations

data ButtonVariant
  = ButtonVariant Variant
  | OutlinedButtonVariant Variant
  | LinkButtonVariant

derive instance Generic ButtonVariant _
derive instance Eq ButtonVariant
instance Show ButtonVariant where
  show (ButtonVariant a)         = (kebabCase <<< genericShow) a
  show (OutlinedButtonVariant a) = (append "outline-" <<< kebabCase <<< genericShow) a
  show LinkButtonVariant         = "link"

----------------------------------------------------------------------

-- | Common sizing values used by various Bootstrap components
-- |
-- | Bootstrap components using sizing use:
-- |    * only 3 possible values
-- |    * `MediumSize` value by default
-- |    * (not to be confused with Bootstrap className utilities also using
-- |      a measuring scale)
-- |
-- | Examples:
-- |    * https://getbootstrap.com/docs/4.1/components/input-group/#sizing
-- |    * https://getbootstrap.com/docs/4.1/components/button-group/#sizing
data Sizing
  = SmallSize
  | MediumSize
  | LargeSize

derive instance Generic Sizing _
derive instance Eq Sizing
instance Show Sizing where
  show SmallSize  = "sm"
  show MediumSize = "md"
  show LargeSize  = "lg"

----------------------------------------------------------------------

-- | Theme values used by Bootstrap Spinner
-- |
-- | https://getbootstrap.com/docs/4.4/components/spinners/
data SpinnerTheme
  = BorderTheme
  | GrowTheme

derive instance Generic SpinnerTheme _
derive instance Eq SpinnerTheme
instance Show SpinnerTheme where
  show BorderTheme = "border"
  show GrowTheme   = "grow"

----------------------------------------------------------------------

-- | Effect used on React Tooltip
-- |
-- | https://github.com/wwayne/react-tooltip#options
data TooltipEffect
 = FloatEffect
 | SolidEffect
----------------------------------------------------------------------
derive instance Generic TooltipEffect _
derive instance Eq TooltipEffect
instance Show TooltipEffect where
  show FloatEffect = "float"
  show SolidEffect = "solid"

----------------------------------------------------------------------

-- | Generic enum type used by various libraries and components
data Position
  = Top
  | Right
  | Left
  | Bottom

derive instance Generic Position _
derive instance Eq Position
instance Show Position where show = kebabCase <<< genericShow

----------------------------------------------------------------------

-- | Position used on React Tooltip
-- |
-- | https://github.com/wwayne/react-tooltip#options
data TooltipPosition
  = TooltipPosition Position
  | AutomaticPosition

derive instance Generic TooltipPosition _
derive instance Eq TooltipPosition
instance Show TooltipPosition where
  show (TooltipPosition a)       = (kebabCase <<< genericShow) a
  show AutomaticPosition         = ""

----------------------------------------------------------------------

-- | Elevation measure scale values used on various custom components
-- | and properties
-- |
-- | Example: https://material.io/design/environment/elevation.html
data Elevation
  = Level0
  | Level1
  | Level2

derive instance Generic Elevation _
derive instance Eq Elevation
instance Show Elevation where show = kebabCase <<< genericShow

----------------------------------------------------------------------

-- | Modal custom sizing used by Bootstrap for its modals
-- |
-- | https://getbootstrap.com/docs/4.6/components/modal/#optional-sizes
data  ModalSizing
  = SmallModalSize
  | MediumModalSize
  | LargeModalSize
  | ExtraLargeModalSize

derive instance Generic ModalSizing _
derive instance Eq ModalSizing
instance Show ModalSizing where
  show SmallModalSize       = "modal-sm"
  show MediumModalSize      = ""
  show LargeModalSize       = "modal-lg"
  show ExtraLargeModalSize  = "modal-xl"
