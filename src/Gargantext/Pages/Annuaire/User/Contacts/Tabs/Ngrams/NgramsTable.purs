-- TODO copy of Gargantext.Pages.Corpus.Tabs.Ngrams.NgramsTable where
module Gargantext.Pages.Annuaire.User.Contacts.Tabs.Ngrams.NgramsTable
  (Mode(..), ngramsTableSpec)
  where

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import React (ReactElement)
import React as React
import Thermite (Render, Spec, createClass, defaultPerformAction, simpleSpec)

import Gargantext.Components.NgramsTable as NT
import Gargantext.Prelude
import Gargantext.Config (PTabNgramType(..), Offset, Limit, End(..), Path(..), TabSubType(..), TabType(..), toUrl)
import Gargantext.Config.REST (get)
import Gargantext.Components.Loader as Loader
import Gargantext.Pages.Annuaire.User.Contacts.Types (Contact)

data Mode = Patents | Books | Communication

derive instance genericMode :: Generic Mode _

instance showMode :: Show Mode where
  show = genericShow

derive instance eqMode :: Eq Mode

type Props = NT.Props Contact Mode

type PageParams = NT.PageParams Mode

getTable :: PTabNgramType -> Maybe Int -> Offset -> Limit -> Aff NT.NgramsTable
getTable tab nodeId offset limit =
  get $ toUrl Back (Ngrams (TabPairing (TabNgramType tab))
                           offset limit Nothing) nodeId

modeTabType :: Mode -> PTabNgramType
modeTabType Patents = PTabPatents
modeTabType Books = PTabBooks
modeTabType Communication = PTabCommunication

loadPage :: PageParams -> Aff NT.NgramsTable
loadPage {nodeId, mode, params: {offset, limit}} =
  getTable (modeTabType mode) (Just nodeId) offset limit
  -- TODO this ignores orderBy

ngramsLoaderClass :: Loader.LoaderClass PageParams NT.NgramsTable
ngramsLoaderClass = Loader.createLoaderClass "ContactsNgramsLoader" loadPage

ngramsLoader :: Loader.Props' PageParams NT.NgramsTable -> ReactElement
ngramsLoader props = React.createElement ngramsLoaderClass props []

ngramsTableClass :: Loader.InnerClass PageParams NT.NgramsTable
ngramsTableClass = createClass "ContactsNgramsTable" NT.ngramsTableSpec NT.initialState

ngramsTableSpec :: Spec {} Props Void
ngramsTableSpec = simpleSpec defaultPerformAction render
  where
    render :: Render {} Props Void
    render _ {path: nodeId, mode} _ _ =
      -- TODO: ignored loaded
      [ ngramsLoader { path: NT.initialPageParams nodeId mode
                     , component: ngramsTableClass
                     } ]
