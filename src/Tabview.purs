module Tabview where

import Authorview as AV
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Data.Array (fold)
import Data.Either (Either(..))
import Data.Lens (Lens', Prism', lens, over, prism, view)
import Data.List (List, fromFoldable)
import Data.Tuple (Tuple(..))
import DocView as DV
import Network.HTTP.Affjax (AJAX)
import Prelude hiding (div)
import React.DOM (a, div, li, text, ul)
import React.DOM.Props (_data, _id, aria, className, href, role)
import Sourceview as SV
import Tab (tabs)
import Tab as Tab
import Termsview as TV
import Thermite (Render, Spec, _performAction, _render, defaultPerformAction, defaultRender, focus, focusState, simpleSpec, withState)

data Action
  =  DocviewA DV.Action
  | SourceviewA SV.Action
  | AuthorviewA AV.Action
  | TermsviewA TV.Action
  | TabViewA Tab.Action
  | NoOp

type State =
  { docview :: DV.State
  , authorview :: AV.State
  , sourceview :: SV.State
  , termsview :: TV.State
  , activeTab :: Int
  }

initialState :: State
initialState =
  { docview : DV.tdata
  , authorview : AV.initialState
  , sourceview : SV.initialState
  , termsview : TV.initialState
  , activeTab : 0
  }

_doclens :: Lens' State DV.State
_doclens = lens (\s -> s.docview) (\s ss -> s {docview = ss})


_authorlens :: Lens' State AV.State
_authorlens = lens (\s -> s.authorview) (\s ss -> s {authorview = ss})


_sourcelens :: Lens' State SV.State
_sourcelens = lens (\s -> s.sourceview) (\s ss -> s {sourceview = ss})


_termslens :: Lens' State TV.State
_termslens = lens (\s -> s.termsview) (\s ss -> s {termsview = ss})


_tablens :: Lens' State Tab.State
_tablens = lens (\s -> s.activeTab) (\s ss -> s {activeTab = ss})

_tabAction :: Prism' Action Tab.Action
_tabAction = prism TabViewA \ action ->
  case action of
    TabViewA laction -> Right laction
    _-> Left action

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

docPageSpec :: forall eff props. Spec (dom :: DOM, console :: CONSOLE, ajax :: AJAX | eff) State props Action
docPageSpec = focus _doclens _docAction DV.layoutDocview

authorPageSpec :: forall eff props. Spec (dom :: DOM, console::CONSOLE, ajax :: AJAX | eff) State  props Action
authorPageSpec = focus _authorlens _authorAction AV.authorSpec

sourcePageSpec :: forall eff props. Spec (dom :: DOM, console :: CONSOLE, ajax :: AJAX | eff) State props Action
sourcePageSpec = focus _sourcelens _sourceAction SV.sourceSpec

termsPageSpec :: forall eff props. Spec (dom :: DOM, console :: CONSOLE, ajax :: AJAX | eff) State props Action
termsPageSpec = focus _termslens _termsAction TV.termsSpec

docPageActionSpec :: forall eff props. Spec (dom :: DOM, console :: CONSOLE, ajax :: AJAX | eff) State props Action
docPageActionSpec = simpleSpec (view _performAction docPageSpec) defaultRender

sourcePagesActionSpec :: forall eff props. Spec (dom :: DOM, console::CONSOLE, ajax :: AJAX | eff) State props Action
sourcePagesActionSpec = simpleSpec (view _performAction sourcePageSpec) defaultRender

authorPageActionSpec :: forall eff props. Spec (dom :: DOM, console :: CONSOLE, ajax :: AJAX | eff) State props Action
authorPageActionSpec = simpleSpec (view _performAction authorPageSpec) defaultRender

termsPageActionSpec :: forall eff props. Spec (dom :: DOM, console :: CONSOLE, ajax :: AJAX | eff) State props Action
termsPageActionSpec = simpleSpec (view _performAction termsPageSpec) defaultRender



tab1 :: forall eff props. Spec ( dom :: DOM, console :: CONSOLE, ajax :: AJAX
    | eff) State props Action
tab1 = tabs _tablens _tabAction $ fromFoldable [ Tuple "Doc View" docPageSpec
      , Tuple "Author View" authorPageSpec
      , Tuple "Source View" sourcePageSpec
      , Tuple "Terms View" termsPageSpec
]
