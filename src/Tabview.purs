module Tabview where

import Authorview as AV
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Data.Either (Either(..))
import Data.Lens (Prism', prism)
import DocView as DV
import Network.HTTP.Affjax (AJAX)
import Prelude hiding (div)
import Sourceview as SV
import Termsview as TV
import Thermite (PerformAction, Render, Spec, focus, modifyState, simpleSpec)


data Action
  =  DocviewA DV.Action
  | SourceviewA SV.Action
  | AuthorviewA AV.Action
  | TermsviewA TV.Action


type State = String

initialState :: State
initialState = ""




_docAction :: Prism' Action DV.Action
_docAction = prism DocviewA \ action ->
  case action of
    DocviewA laction -> Right laction
    _-> Left action

_sourceAction :: Prism' Action SV.Action
_sourceAction = prism SourceviewA \ action ->
  case action of
    SourceviewA laction -> Right laction
    _-> Left action

_authorAction :: Prism' Action AV.Action
_authorAction = prism AuthorviewA \ action ->
  case action of
    AuthorviewA laction -> Right laction
    _-> Left action


_termsAction :: Prism' Action TV.Action
_termsAction = prism TermsviewA \ action ->
  case action of
    TermsviewA laction -> Right laction
    _-> Left action



-- docPageSpec :: forall eff props. Spec (console :: CONSOLE, ajax :: AJAX | eff) State props Action
-- docPageSpec = focus _fackLens _docAction DV.matchesSpec

-- authorPagesSpec :: forall eff props. Spec (console::CONSOLE, ajax :: AJAX | eff) State  props Action
-- authorPagesSpec = focus _fackLens _profilePageAction AV.profilePageSpec


-- sourcePageSpec :: forall eff props. Spec (console :: CONSOLE, ajax :: AJAX | eff) State props Action
-- sourcePageSpec = focus _fackLens _messageAction SV.myMessagesSpec
