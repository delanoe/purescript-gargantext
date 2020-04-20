module Gargantext.Router where

import Prelude
import Data.Foldable (oneOf)
import Data.Int (floor)
import Routing.Match (Match, lit, num, str)
import Gargantext.Routes (AppRoute(..))
import Gargantext.Types (SessionId(..))

router :: Match AppRoute
router = oneOf
  [ Login            <$   route "login"
  , Folder           <$> (route "folder"     *> sid) <*> int
  , FolderPrivate    <$> (route "folderPrivate"     *> sid) <*> int
  , FolderPublic     <$> (route "folderPublic"     *> sid) <*> int
  , FolderShared     <$> (route "folderShared"     *> sid) <*> int
  , Team             <$> (route "team"     *> sid) <*> int
  , CorpusDocument   <$> (route "corpus"     *> sid) <*> int
                        <*> (lit "list" *> int)
                        <*> (lit "document" *> int)
  , Corpus            <$> (route "corpus"     *> sid) <*> int
     , Document       <$> (route "list"      *> sid) <*> int 
                        <*> (lit "document" *> int)
     , Dashboard      <$> (route "dashboard" *> sid) <*> int
     , PGraphExplorer <$> (route "graph"     *> sid) <*> int
     , Texts          <$> (route "texts"     *> sid) <*> int
     , Lists          <$> (route "lists"     *> sid) <*> int
    , ContactPage     <$> (route "annuaire"  *> sid) <*> int
                          <*> (lit "contact" *> int)
  , Annuaire          <$> (route "annuaire"  *> sid) <*> int
    , UserPage        <$> (route "user"      *> sid) <*> int
  , Home              <$   lit ""
  ]
 where
    route str      = lit "" *> lit str

    int :: Match Int
    int = floor <$> num

    sid :: Match SessionId
    sid = SessionId <$> str

