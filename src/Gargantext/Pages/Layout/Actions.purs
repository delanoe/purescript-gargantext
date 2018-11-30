-- | Module Description

module Gargantext.Pages.Layout.Actions where

import Data.Either                                     (Either(..))
import Data.Maybe                                      (Maybe(..))
import Data.Lens                                       (Prism', prism)
import Effect.Class                                    (liftEffect)
import Thermite                                        (PerformAction, modifyState, modifyState_)
import Routing.Hash                                    (setHash)

import Gargantext.Components.Login                  as LN
import Gargantext.Components.Modals.Modal              (modalShow)
import Gargantext.Pages.Annuaire             as Annuaire
import Gargantext.Pages.Annuaire.User.Contacts      as C
import Gargantext.Pages.Corpus.Document       as D
import Gargantext.Pages.Corpus.Graph     as GE
import Gargantext.Pages.Layout.Specs.AddCorpus      as AC
import Gargantext.Pages.Layout.Specs.Search         as S
import Gargantext.Pages.Layout.States                  (AppState)
import Gargantext.Prelude
import Gargantext.Router                               (Routes)

------------------------------------------------------------------------

data Action
  = LoginA     LN.Action
  | SetRoute   Routes
    | SearchA    S.Action
    | Search             String
    | AddCorpusA AC.Action
    | GraphExplorerA     GE.Action
    | DocumentViewA D.Action
  | AnnuaireAction     Annuaire.Action
    | UserPageA  C.Action
  | Go
  | ShowLogin
  | Logout
  | ShowAddcorpus
  | ShowTree 


performAction :: PerformAction AppState {} Action
performAction (SetRoute route)  _ _ = void do
  modifyState $ _ {currentRoute = pure route}
performAction (Search s)  _ _ = void do
  modifyState $ _ {search = s}

performAction (ShowTree)  _ (state) = void do -- TODO
  modifyState $ _ {showTree = not (state.showTree)}

performAction (ShowLogin)  _ _ = void do
  liftEffect $ modalShow "loginModal"
  modifyState $ _ {showLogin = true}

performAction Logout _ _ = do
  loginState <- liftEffect do
    LN.setAuthData Nothing
    setHash "/"
    LN.initialState
  modifyState_ $ _ {currentRoute = Nothing, loginState = loginState}

---------------------------------------------------------
-- TODO chose one of them
performAction (ShowAddcorpus)  _ _ = void do
  liftEffect $ modalShow "addCorpus"
  modifyState $ _ {showCorpus = true}

performAction Go  _ _ = void do
  liftEffect $ modalShow "addCorpus"
  modifyState $ _ {showCorpus = true}
 -- _ <- lift $ setHash "/addCorpus"
  --modifyState id
---------------------------------------------------------

performAction (LoginA        _) _ _ = pure unit
performAction (AddCorpusA    _) _ _ = pure unit
performAction (SearchA       _) _ _ = pure unit
performAction (UserPageA     _) _ _ = pure unit
performAction (DocumentViewA _) _ _ = pure unit
performAction (GraphExplorerA     _) _ _ = pure unit
performAction (AnnuaireAction     _) _ _ = pure unit

----------------------------------------------------------

_loginAction :: Prism' Action LN.Action
_loginAction = prism LoginA \action ->
  case action of
    LoginA caction -> Right caction
    _-> Left action

_addCorpusAction :: Prism' Action AC.Action
_addCorpusAction = prism AddCorpusA \action ->
  case action of
    AddCorpusA caction -> Right caction
    _-> Left action

_searchAction :: Prism' Action S.Action
_searchAction = prism SearchA \action ->
  case action of
    SearchA caction -> Right caction
    _-> Left action

_userPageAction :: Prism' Action C.Action
_userPageAction = prism UserPageA \action ->
  case action of
    UserPageA caction -> Right caction
    _-> Left action

_annuaireAction :: Prism' Action Annuaire.Action
_annuaireAction = prism AnnuaireAction \action ->
  case action of
       AnnuaireAction a -> Right a
       _                -> Left  action

_documentViewAction :: Prism' Action D.Action
_documentViewAction = prism DocumentViewA \action ->
  case action of
    DocumentViewA caction -> Right caction
    _-> Left action

_graphExplorerAction :: Prism' Action GE.Action
_graphExplorerAction = prism GraphExplorerA \action ->
  case action of
    GraphExplorerA caction -> Right caction
    _-> Left action
