module Gargantext.Components.Tab where

import Prelude hiding (div)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Reactix as R
import Reactix.DOM.HTML as H

import Gargantext.Utils.Reactix as R2

here :: R2.Here
here = R2.here "Gargantext.Components.Tab"

type TabsProps = (
    selected :: Int
  , tabs     :: Array (Tuple String R.Element)
  )

tabs :: R2.Leaf TabsProps
tabs props = R.createElement tabsCpt props []

-- this is actually just the list of tabs, not the tab contents itself
tabsCpt :: R.Component TabsProps
tabsCpt = here.component "tabs" cpt where
  cpt props _ = do
    (activeTab /\ setActiveTab) <- R.useState' props.selected
    pure $ H.div {}
      [ H.nav {}
        [ H.br {}
        , H.div { className: "nav nav-tabs", title: "Search result" }
          (mapWithIndex (button setActiveTab activeTab) props.tabs) 
        ]
      , H.div { className: "tab-content" }
        (mapWithIndex (item activeTab) props.tabs)
      ]
  button setActiveTab selected index (name /\ _) =
    H.a { className, on: { click } } [ H.text name ] where
      eq = index == selected
      className = "nav-item nav-link" <> (if eq then " active" else "")
      click e = setActiveTab (const index)
  item selected index (_ /\ cpt') = tab { selected, index } [ cpt' ]

-- TODO: document what these are (selection, item indices)
type TabProps = ( selected :: Int, index :: Int )

tab :: R2.Component TabProps
tab = R.createElement tabCpt

-- | A tab only shows its contents if it is currently selected
tabCpt :: R.Component TabProps
tabCpt = R.staticComponent "G.C.Tab.tab" cpt
  where
    cpt { selected, index } children = H.div { className } children'
      where
        same = selected == index
        className = "tab-pane" <> (if same then "show active" else "fade")
        children' = if same then children else []

