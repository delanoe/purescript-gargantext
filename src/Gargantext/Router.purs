module Gargantext.Router where

import Prelude

import Data.Foldable (oneOf)
import Data.Int (floor)
import Gargantext.Routes (AppRoute(..))
import Gargantext.Types (SessionId(..))
import Routing.Match (Match, lit, num, str)

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
     , CorpusCode     <$> (route "corpusCode" *> sid) <*> int
     , Document       <$> (route "list"      *> sid) <*> int
                        <*> (lit "document" *> int)
     , Dashboard      <$> (route "dashboard" *> sid) <*> int
     , PGraphExplorer <$> (route "graph"     *> sid) <*> int
     , PhyloExplorer  <$> (route "phylo"     *> sid) <*> int
     , Texts          <$> (route "texts"     *> sid) <*> int
     , Lists          <$> (route "lists"     *> sid) <*> int
    , ContactPage     <$> (route "annuaire"  *> sid) <*> int
                          <*> (lit "contact" *> int)
  , Annuaire          <$> (route "annuaire"  *> sid) <*> int
    , UserPage        <$> (route "user"      *> sid) <*> int

  , RouteFrameWrite    <$> (route "write"    *> sid) <*> int
  , RouteFrameCalc     <$> (route "calc"     *> sid) <*> int
  , RouteFrameCode     <$> (route "code"     *> sid) <*> int
  , RouteFrameVisio    <$> (route "visio"    *> sid) <*> int
  , RouteFile          <$> (route "file"     *> sid) <*> int
  , Home              <$   lit ""
  ]
 where
    route str      = lit "" *> lit str

    int :: Match Int
    int = floor <$> num

    sid :: Match SessionId
    sid = SessionId <$> str
