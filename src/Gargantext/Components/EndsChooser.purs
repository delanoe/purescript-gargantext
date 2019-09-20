-- | 
module Gargantext.Components.EndsChooser
  -- (
  -- )
  where

import Prelude (($), pure)
import Reactix as R
import Reactix.DOM.HTML as H
import Gargantext.Config (Backend, Ends, defaultEnds, defaultEnds')
import Gargantext.Utils.Reactix as R2

type Props = ( ends :: R.State Ends )

useEnds :: R.Hooks (R.State Ends)
useEnds = R.useState' defaultEnds

endsChooser :: Record Props -> R.Element
endsChooser props = R.createElement endsChooserCpt props []

endsChooserCpt :: R.Component Props
endsChooserCpt = R.hooksComponent "G.C.EndsChooser.endsChooser" cpt
  where
    cpt {ends} _ = do
      pure $ R.fragment []
--     el = R.hooksComponent "EndConfigChooserCpt" cpt
--     cpt {state} _children = do
--       R.useEffect $ pure $
--         if (configState /= state) then do
--           _ <- log2 "update state: " configState
--           _ <- d $ ConfigStateA $ C.UpdateState configState
--           _ <- log2 "logout" ""
--           d $ Logout
--         else
--           pure $ unit

--       pure $ H.span {}
--         [ endConfigChooser (configState /\ setConfigState)
--         , H.span {className: "text-info"}
--           [ H.text $ C.endConfigDisplayName configState.endConfig ]
--         , H.span {className: "text-danger"}
--           [ H.text $ C.endConfigDisplayName state.endConfig ]
--         ]

-- endConfigChooser :: R.State Ends -> R.Element
-- endConfigChooser (configState /\ setConfigState) = R.createElement el {} []
--   where
--     el = R.hooksComponent "EndConfigChooser" cpt
--     cpt {} _ = do
--       -- NOTE Need to rebind the component after rerender
--       R.useEffect do
--         _ <- pure $ createDropdown "end-config-chooser"
--         pure $ pure unit

--       pure $ H.li {className: "dropdown"}
--         [ H.a { className: "navbar-text dropdown-toggle"
--               , href: "#"
--               , role: "button"
--               , data: {toggle: "dropdown"}
--               , id: "end-config-chooser"
--               }
--           [ H.text $ C.endConfigDisplayName configState.endConfig ]
--         , H.ul { className: "dropdown-menu"
--                } (liItem <$> C.endConfigOptions)
--         ]

--     liItem :: C.EndConfigOption -> R.Element
--     liItem {endConfig, displayName} =
--       H.li {on: {click: onClick endConfig}}
--       [ H.a {href: "#"} [H.text displayName] ]

--     onClick endConfig = \_ -> do
--       log2 "set end config" endConfig
--       setConfigState $ \st -> st {endConfig = endConfig}

-- type BackendProps = ( ends :: R.State Ends, backend :: Backend )

-- backendCpt :: R.Component BackendProps
-- backendCpt = R.hooksComponent "G.C.EndsChooser.backend" cpt
--   where
--     cpt {ends, backend} _ = do
      

