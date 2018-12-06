module Gargantext.Components.Tab where

import Prelude hiding (div)

import Data.Array (fold)
import Data.Lens (Lens', Prism', over, view)
import Data.List (List, mapWithIndex, toUnfoldable)
import Data.Tuple (Tuple(..))
import React (ReactElement)
import React.DOM (a, div, nav, text)
import React.DOM.Props (className, onClick)
import Thermite ( PerformAction, Render, Spec
                , _render, modifyState, focus
                , simpleSpec, withState)

type State = { activeTab :: Int }

data Action = ChangeTab Int

tabs :: forall state props action.
  Lens' state State -> Prism' action Action
  -> List (Tuple String (Spec state props action))
                      -> Spec state props action
tabs l p ls = withState \st ->
  let {activeTab} = view l st in
  fold
  [ focus l p $ simpleSpec performAction (render activeTab  ls)
  , wrapper $ fold $ mapWithIndex        (   tab activeTab) ls
  ]
  where
    performAction :: forall props.
      PerformAction State props Action
    performAction (ChangeTab activeTab) _ _ =
      void $ modifyState $ const {activeTab}
    wrapper = over _render \render d p s c ->
      [div [className "tab-content"] $ render d p s c]

tab :: forall state props action. 
  Int -> Int -> Tuple String (Spec state props action)
                           -> Spec state props action
tab sid iid (Tuple name spec) = over _render tabRender spec
  where
    tabRender renderer d p s c =
      [ div [ className $ "tab-pane " <>
        if sid ==iid 
           then " show active" 
           else " fade"] $ renderer d p s c
      ]


render :: forall state props action.
  Int -> List (Tuple String (Spec state props action))
                          -> Render State props Action
render at ls d p s c =
  [ nav []
    [ div [className "nav nav-tabs"]
      $ toUnfoldable $ mapWithIndex (item at) ls
    ]
  ]
  where
    item :: forall a. Int -> Int -> (Tuple String a) -> ReactElement
    item sid iid (Tuple name _) =
      a [className $ "nav-item nav-link" <>
      if sid == iid
         then " active"
         else "", onClick \e -> d $ ChangeTab iid] [text name]
