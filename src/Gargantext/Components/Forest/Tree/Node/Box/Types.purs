module Gargantext.Components.Forest.Tree.Node.Box.Types where

import DOM.Simple as DOM
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Gargantext.Components.Forest.Tree.Node.Action (Action)
import Gargantext.Components.Forest.Tree.Node.Settings (NodeAction)
import Gargantext.Prelude (Unit)
import Gargantext.Sessions (Session)
import Gargantext.Types (FrontendError, ID, Name)
import Gargantext.Types as GT
import Toestand as T

type CommonProps =
  ( dispatch :: Action -> Aff Unit
  , session  :: Session
  , handed   :: GT.Handed
  )

type NodePopupProps =
  ( errors         :: T.Box (Array FrontendError)
  , id             :: ID
  , name           :: Name
  , nodeType       :: GT.NodeType
  , onPopoverClose :: DOM.Element -> Effect Unit
  | CommonProps
  )

type NodePopupS =
  ( action   :: Maybe NodeAction
  , id       :: ID
  , name     :: Name
  , nodeType :: GT.NodeType
  )


