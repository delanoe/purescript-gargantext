-- | Module Description

module Gargantext.Pages.Layout.Actions where

import Control.Monad.Cont.Trans                        (lift)
import Data.Either                                     (Either(..))
import Data.Lens                                       (Prism', prism)
import Effect.Class                                    (liftEffect)
import Thermite                                        (PerformAction, modifyState, modifyState_)

import Gargantext.Config (defaultRoot)
import Gargantext.Components.Login                  as LN
import Gargantext.Components.Modals.Modal              (modalShow)
import Gargantext.Components.Tree                   as Tree
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
  = Initialize
  | LoginA     LN.Action
  | SetRoute   Routes
  | TreeViewA          Tree.Action
    | SearchA    S.Action
    | Search             String
    | AddCorpusA AC.Action
    | GraphExplorerA     GE.Action
    | DocumentViewA D.Action
  | AnnuaireAction     Annuaire.Action
    | UserPageA  C.Action
  | Go
  | ShowLogin
  | ShowAddcorpus
  | ShowTree 


performAction :: PerformAction AppState {} Action
performAction (SetRoute route)  _ _ = void do
  modifyState $ _ {currentRoute = pure route}
performAction (Search s)  _ _ = void do
  modifyState $ _ {search = s}

performAction (ShowTree)  _ (state) = void do
  modifyState $ _ {showTree = not (state.showTree)}

performAction (ShowLogin)  _ _ = void do
  liftEffect $ modalShow "loginModal"
  modifyState $ _ {showLogin = true}

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

performAction Initialize  _ state = do
  _ <- logs "loading Initial nodes"
  case state.initialized of
    false -> do
      d <- lift $ Tree.loadNode defaultRoot
      modifyState_ $ _ { initialized = true, ntreeState = {state: d} }
    _ -> pure unit

performAction (LoginA        _) _ _ = pure unit
performAction (AddCorpusA    _) _ _ = pure unit
performAction (SearchA       _) _ _ = pure unit
performAction (UserPageA     _) _ _ = pure unit
performAction (DocumentViewA _) _ _ = pure unit
performAction (TreeViewA          _) _ _ = pure unit
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

_treeAction :: Prism' Action Tree.Action
_treeAction = prism TreeViewA \action ->
  case action of
    TreeViewA caction -> Right caction
    _-> Left action

_graphExplorerAction :: Prism' Action GE.Action
_graphExplorerAction = prism GraphExplorerA \action ->
  case action of
    GraphExplorerA caction -> Right caction
    _-> Left action
