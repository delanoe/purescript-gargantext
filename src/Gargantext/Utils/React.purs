module Gargantext.Utils.React where

import Prelude
import Data.Array ( (!!) )
import Data.FoldableWithIndex ( foldMapWithIndex )
import Data.Maybe ( fromMaybe )
import React ( ReactElement, Children )
import Reactix as R
import Unsafe.Coerce ( unsafeCoerce )

-- TODO: Upgrade thermite and reapply our changes or upstream them and get rid of this
type WithChildren props = { children :: Children | props }

wrap :: (Array ReactElement -> ReactElement) -> ReactElement -> ReactElement
wrap f e = f [e]

crapify :: R.Element -> ReactElement
crapify = unsafeCoerce

-- many ::
--   forall props extra state action.
--      Spec state { extra :: extra | WithIndex props } action
--   -> Spec state { props :: (Array props), extra :: extra } action
-- many items spec = Spec { performAction, render }
--   where
--     -- performAction :: PerformAction
--     render d p s c = foldMapWithIndex childSpec p.props
--     childSpec props i = cmapProps (\_ -> props { index = i }) spec

-- many
--   :: forall props state action
--    . (Int -> Spec state props action)
--   -> Spec state (List props) (Tuple Int action)
-- foreach f = Spec  { performAction: performAction
--     , render: render
--     }
--   where
--     performAction :: PerformAction (List state) props (Tuple Int action)
--     performAction (Tuple i a) p sts =
--         for_ (sts !! i) \st ->
--           case f i of
--             Spec s -> forever (transform (_ >>= (_ !! i)))
--                       `transformCoTransformL` s.performAction a p st
--                       `transformCoTransformR` forever (transform (modifying i))
--       where
--         modifying :: Int -> (state -> state) -> List state -> List state
--         modifying j g sts' = fromMaybe sts' (modifyAt j g sts')

--     render :: Render (List state) props (Tuple Int action)
--     render k p sts _ = foldWithIndex (\i st els -> case f i of Spec s -> els <> s.render (k <<< Tuple i) p st []) sts []

--     foldWithIndex :: forall a r. (Int -> a -> r -> r) -> List a -> r -> r
--     foldWithIndex g = go 0
--       where
--       go _ Nil         r = r
--       go i (Cons x xs) r = go (i + 1) xs (g i x r)
