module Gargantext.Components.Forest.Tree.Node.Action.Add where

import Data.Array (length, head)
import Data.Maybe (Maybe(..), fromMaybe)
-- import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, launchAff)
import Effect.Uncurried (mkEffectFn1)
import FFI.Simple ((..))
import Gargantext.Components.Forest.Tree.Node.Action (Action(..), ID, Name)
import Gargantext.Components.Forest.Tree.Node (SettingsBox(..), settingsBox)
import Gargantext.Types (NodeType(..), readNodeType)
import Gargantext.Utils.Reactix as R2
import Prelude (Unit, bind, const, discard, map, pure, show, ($), (<>), (>), (<<<))
import Reactix as R
import Reactix.DOM.HTML as H

-- START Create Node

data NodePopup = CreatePopup | NodePopup

type CreateNodeProps =
  ( id       :: ID
  , name     :: Name
  , nodeType :: NodeType
  )

createNodeView :: (Action -> Aff Unit)
               -> Record CreateNodeProps
               -> R.State (Maybe NodePopup)
               -> Array NodeType
               -> R.Element
createNodeView d p@{nodeType} (_ /\ setPopupOpen) nodeTypes = R.createElement el p []
  where
    el = R.hooksComponent "CreateNodeView" cpt
    cpt {id, name} _ = do
      nodeName <- R.useState' "Default Name"
      nodeType' <- R.useState' $ fromMaybe NodeUser $ head nodeTypes
      pure $ H.div {}
          [ panelBody   readNodeType nodeName nodeType'
          , panelFooter nodeName nodeType'
          ]
      where
        panelBody :: (String -> NodeType)
                  -> R.State String
                  -> R.State NodeType
                  -> R.Element
        panelBody readIt (_ /\ setNodeName) (nt /\ setNodeType) =
          H.div {className: "panel-body"}
          [ H.div {className: "row"}
            [ H.div {className: "col-md-10"}
              [ H.form {className: "form-horizontal"} $ maybeChoose <> maybeEdit ]
              ]
            ]
              where
                SettingsBox {edit} = settingsBox nt
                maybeEdit = [ if edit then
                                H.div {className: "form-group"}
                                   [ H.input { type: "text"
                                             , placeholder: "Node name"
                                             , defaultValue: "Write Name here"
                                             , className: "form-control"
                                             , onInput: mkEffectFn1 $ setNodeName <<< const <<< R2.unsafeEventValue
                                            }
                                   ]
                              else
                                H.div {} []
                             ]

                maybeChoose = [ if length nodeTypes > 1 then
                                  R.fragment [
                                    H.div {className: "form-group"} $ [
                                       R2.select { className: "form-control"
                                                 , onChange: mkEffectFn1 $ setNodeType <<< const <<< readIt <<< R2.unsafeEventValue
                                                 }
                                       (map (\opt -> H.option {} [ H.text $ show opt ]) nodeTypes)
                                         ]
                                         -- , showConfig nt
                                    ]
                                else
                                H.button { className : "btn btn-primary"
                                           , type : "button"
                                           , onClick : mkEffectFn1 $ \_ -> setNodeType ( const
                                                                                       $ fromMaybe nt 
                                                                                       $ head nodeTypes
                                                                                       )
                                           } []
                               ]



        panelFooter :: R.State String  -> R.State NodeType -> R.Element
        panelFooter (name' /\ _) (nt /\ _) =
          H.div {className: "panel-footer"}
          [ H.button {className: "btn btn-primary text-center"
                     , type: "button"
                     , onClick: mkEffectFn1 $ \_ -> do
                         setPopupOpen $ const Nothing
                         launchAff    $ d $ CreateSubmit name' nt
                     } [H.text "Add"]
          ]

-- END Create Node


showConfig :: NodeType -> R.Element
showConfig NodeUser      = H.div {} []
showConfig FolderPrivate = H.div {} [H.text "This folder will be private only"]
showConfig FolderShared  = H.div {} [H.text "This folder will be shared"]
showConfig FolderPublic  = H.div {} [H.text "This folder will be public"]
showConfig nt            = H.div {} [H.h4  {} [H.text $ "Config of " <> show nt ]]


