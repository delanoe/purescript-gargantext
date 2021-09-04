module Gargantext.Components.ListSelection where

import Gargantext.Prelude

import Data.Array as A
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Gargantext.Components.Forest.Tree.Node.Tools (formChoiceSafe)
import Gargantext.Types (ListId)
import Gargantext.Utils.Reactix as R2
import Reactix as R
import Reactix.DOM.HTML as H
import Toestand as T

here :: R2.Here
here = R2.here "Gargantext.Components.ListSelection"

data Selection = MyListsFirst | OtherListsFirst | SelectedLists (Array ListId)
derive instance Generic Selection _
instance Show Selection where
  show MyListsFirst = "My lists first"
  show OtherListsFirst = "Other lists first"
  show (SelectedLists _) = "Selected lists"
instance Eq Selection where eq = genericEq
instance Read Selection where
  read "My lists first" = Just MyListsFirst
  read "Other lists first" = Just OtherListsFirst
  read "Selected lists" = Just $ SelectedLists []
  read _ = Nothing

type Props =
  ( selection   :: T.Box Selection )

selection :: R2.Component Props
selection = R.createElement selectionCpt
selectionCpt :: R.Component Props
selectionCpt = here.component "selection" cpt where
  cpt { selection } _ = do
    pure $ H.div {}
      [ formChoiceSafe [ MyListsFirst
                       , OtherListsFirst
                       , SelectedLists [] ] MyListsFirst setSelection show
      , selectedIds { selection } []
      ]
    where
      setSelection val = T.write_ val selection

selectedIds :: R2.Component Props
selectedIds = R.createElement selectedIdsCpt
selectedIdsCpt :: R.Component Props
selectedIdsCpt = here.component "selectedIds" cpt where
  cpt { selection } _ = do
    selection' <- T.useLive T.unequal selection

    pure $ case selection' of
      SelectedLists ids -> H.div {} [ idsSelector { ids, selection } [] ]
      _ -> H.div {} []

type IdsSelectorProps =
  ( ids       :: Array ListId
  , selection :: T.Box Selection )

idsSelector :: R2.Component IdsSelectorProps
idsSelector = R.createElement idsSelectorCpt
idsSelectorCpt :: R.Component IdsSelectorProps
idsSelectorCpt = here.component "idsSelector" cpt where
  cpt { ids, selection } _ = do
    R.useEffect' $ do
      here.log2 "[idsSelector] ids" ids
    
    pure $ H.div {} $ map checkbox [1, 2, 3, 4]
    where
      checkbox val = H.div {}
        [ H.input { className: "form-check-input"
                  , on: { click }
                  , type: "checkbox"
                  , value: A.elem val ids }
        , H.label {} [ H.text $ show val ]
        ]
        where
          click _ = do
            let f (SelectedLists lst) =
                  if A.elem val ids
                  then SelectedLists (A.delete val lst)
                  else SelectedLists (A.cons val lst)
                f x = x
            T.modify_ f selection

listTree :: R2.Component IdsSelectorProps
listTree = R.createElement listTreeCpt
listTreeCpt :: R.Component IdsSelectorProps
listTreeCpt = here.component "listTree" cpt where
  cpt { ids, selection } _ = do
    pure $ H.div {} []
