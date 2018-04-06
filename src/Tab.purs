module Tab where

import Data.Array (fold)
import Data.Lens (Lens', Prism', over, view)
import Data.List (List, mapWithIndex, toUnfoldable)
import Data.Tuple (Tuple(..))
import Prelude hiding (div)
import React (ReactElement)
import React.DOM (a, div, nav, text)
import React.DOM.Props (className, onClick)
import Thermite (PerformAction, Render, Spec, _render, cotransform, focus, simpleSpec, withState)

type State = Int

data Action = ChangeTab Int

tabs :: forall eff state props action . Lens' state State -> Prism' action Action -> List (Tuple String (Spec eff state props action)) -> Spec eff state props action
tabs l p ls = withState \st ->
  fold
  [ focus l p $ simpleSpec performAction (render (activeTab st) ls)
  , wrapper $ fold $ mapWithIndex (tab (activeTab st)) ls
  ]
  where
    activeTab = view l
    wrapper = over _render \render d p s c ->
      [div [className "tab-content"] $ render d p s c]

tab :: forall eff state props action. Int -> Int -> Tuple String (Spec eff state props action) -> Spec eff state props action
tab sid iid (Tuple name spec) = over _render tabRender spec
  where
    tabRender renderer d p s c =
      [div [ className $ "tab-pane " <> if sid ==iid then " show active" else " fade"] $ renderer d p s c]


performAction :: forall eff props. PerformAction eff State props Action
performAction (ChangeTab i) _ _ = void do
  cotransform \_ -> i

render :: forall eff state props action. State -> List (Tuple String (Spec eff state props action)) -> Render State props Action
render at ls d p s c =
  [ nav []
    [ div [className "nav nav-tabs"]
      $ toUnfoldable $ mapWithIndex (item at) ls
    ]
  ]
  where
    item :: forall a. Int -> Int -> (Tuple String a) -> ReactElement
    item sid iid (Tuple name _) =
      a [className $ "nav-item nav-link" <> if sid == iid then " active"  else "", onClick \e -> d $ ChangeTab iid] [text name]
