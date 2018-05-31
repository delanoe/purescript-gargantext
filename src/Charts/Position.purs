module Charts.Position
       (
         Position(),
         numberPosition,
         percentPosition,
         relativePosition,
         Align(..),
         TopRelativePosition(..),
         LeftRelativePosition(..)
       ) where

import Prelude (class Show, show, ($), (<>))
import Unsafe.Coerce (unsafeCoerce)

-- | The type `Position` is made to render a css position.
-- | It should be either a `Number`, a `"Number%"` or a `Position` type (`TopRelativePosition` for exemple)
-- | To construct such a type you will have to use one of the smart constructor
foreign import data Position :: Type -> Type

-- | Smart constructor to build a JS Number
numberPosition :: forall r. Number -> Position r
numberPosition = unsafeCoerce

-- | Smart constructor to build a JS Percent
percentPosition :: forall r. Number -> Position r
percentPosition n = unsafeCoerce $ (show n) <> "%"

-- | Smart constructor to build a JS String giving position's detail ("top", "left", ...)
relativePosition :: forall a. Show a => Align a -> Position a
relativePosition (Auto) = unsafeCoerce "auto"
relativePosition (Relative r) = unsafeCoerce $ show r

data Align p = Auto | Relative p

data TopRelativePosition = Top | Middle | Bottom
instance showTopRelativePosition :: Show TopRelativePosition
  where show (Top) = "top"
        show (Middle) = "middle"
        show (Bottom) = "bottom"

data LeftRelativePosition = LeftPos | Center | RightPos
instance showLeftRelativePosition :: Show LeftRelativePosition
  where show (LeftPos) = "left"
        show (Center) = "center"
        show (RightPos) = "right"
