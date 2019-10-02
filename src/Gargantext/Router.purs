module Gargantext.Router where

import Prelude
import Data.Foldable (oneOf)
import Data.Int (floor)
import Routing.Match (Match, lit, num)
import Gargantext.Routes (AppRoute(..))

router :: Match AppRoute
router = oneOf
  [ Login            <$   route "login"
  , Folder           <$> (route "folder" *> int)
  , CorpusDocument   <$> (route "corpus" *> int)
                        <*> (lit "list"  *> int)
                        <*> (lit "document" *> int)
  , Corpus           <$> (route "corpus" *> int)
     , Document       <$> (route "list"  *> int) 
                        <*> (lit "document" *> int)
     , Dashboard      <$> (route "dashboard" *> int)
     , PGraphExplorer <$> (route "graph"    *> int)
     , Texts          <$> (route "texts"    *> int)
     , Lists          <$> (route "lists"    *> int)
  , Annuaire          <$> (route "annuaire" *> int)
    , UserPage        <$> (route "user"     *> int)
    , ContactPage     <$> (route "contact"  *> int)
  , Home              <$   lit ""
  ]
 where
    route str      = lit "" *> lit str
    int :: Match Int
    int = floor <$> num

