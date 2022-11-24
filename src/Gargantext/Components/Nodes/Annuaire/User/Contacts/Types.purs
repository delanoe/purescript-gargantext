module Gargantext.Components.Nodes.Annuaire.User.Contacts.Types where

import Data.Array as A
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Lens (Lens', lens)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.String as S
import Record as Record
import Simple.JSON as JSON
import Type.Proxy (Proxy(..))

import Gargantext.Prelude (class Eq, bind, pure, ($))


-- TODO: should it be a NodePoly HyperdataContact ?
newtype NodeContact =
  NodeContact
  { id        :: Int
  , date      :: Maybe String
  , hyperdata :: HyperdataContact
  , name      :: Maybe String
  , parentId  :: Maybe Int
  , typename  :: Maybe Int
  , userId    :: Maybe Int
  }
derive instance Generic NodeContact _
derive instance Newtype NodeContact _
instance Eq NodeContact where eq = genericEq
instance JSON.ReadForeign NodeContact where
  readImpl f = do
    inst <- JSON.readImpl f
    pure $ NodeContact $ Record.rename parent_idP parentIdP $ Record.rename user_idP userIdP inst

----------------------------------------------------------------------------

newtype Contact' =
  Contact'
  { id :: Int
  , date :: Maybe String
  , hyperdata :: HyperdataContact
  , name :: Maybe String
  , parentId :: Maybe Int
  , typename :: Maybe Int
  , userId :: Maybe Int
  }
derive instance Generic Contact' _
derive instance Newtype Contact' _
instance Eq Contact' where eq = genericEq
instance JSON.ReadForeign Contact' where
  readImpl f = do
    inst <- JSON.readImpl f
    pure $ Contact' $ Record.rename parent_idP parentIdP $ Record.rename user_idP userIdP inst


-- | TODO rename Contact with User
-- and fix shared decodeJson
newtype Contact =
  Contact
  { id :: Int
  , date :: Maybe String
  , hyperdata :: HyperdataUser
  , name :: Maybe String
  , parentId :: Maybe Int
  , typename :: Maybe Int
  , userId :: Maybe Int
  }
derive instance Generic Contact _
derive instance Newtype Contact _
instance Eq Contact where eq = genericEq
instance JSON.ReadForeign Contact where
  readImpl f = do
    inst <- JSON.readImpl f
    pure $ Contact $ Record.rename parent_idP parentIdP $ Record.rename user_idP userIdP inst

----------------------------------------------------------------------------
newtype User =
  User
  { id :: Int
  , date :: Maybe String
  , hyperdata :: HyperdataUser
  , name :: Maybe String
  , parentId :: Maybe Int
  , typename :: Maybe Int
  , userId :: Maybe Int
  }

derive instance Generic User _
derive instance Newtype User _
instance JSON.ReadForeign User where
  readImpl f = do
    inst <- JSON.readImpl f
    pure $ User $ Record.rename parent_idP parentIdP $ Record.rename user_idP userIdP inst

newtype ContactWho =
  ContactWho
  { idWho     :: Maybe String
  , firstName :: Maybe String
  , lastName  :: Maybe String
  , keywords  :: (Array String)
  , freetags  :: (Array String)
  }

derive instance Newtype ContactWho _
derive instance Generic ContactWho _
instance Eq ContactWho where eq = genericEq
instance JSON.ReadForeign ContactWho where
  readImpl f = do
    inst <- JSON.readImpl f

    pure $ ContactWho $ inst { keywords = fromMaybe [] inst.keywords
                             , freetags = fromMaybe [] inst.freetags }
derive newtype instance JSON.WriteForeign ContactWho

defaultContactWho :: ContactWho
defaultContactWho =
  ContactWho {
      idWho: Nothing
    , firstName: Nothing
    , lastName: Nothing
    , keywords: []
    , freetags: []
    }

newtype ContactWhere =
  ContactWhere
  { organization :: (Array String)
  , labTeamDepts :: (Array String)

  , role         :: Maybe String

  , office       :: Maybe String
  , country      :: Maybe String
  , city         :: Maybe String

  , touch        :: Maybe ContactTouch

  , entry        :: Maybe String
  , exit         :: Maybe String }

derive instance Newtype ContactWhere _
derive instance Generic ContactWhere _
instance Eq ContactWhere where eq = genericEq
instance JSON.ReadForeign ContactWhere where
  readImpl f = do
    inst <- JSON.readImpl f
    pure $ ContactWhere $ inst { organization = fromMaybe [] inst.organization
                               , labTeamDepts = fromMaybe [] inst.labTeamDepts }
derive newtype instance JSON.WriteForeign ContactWhere

defaultContactWhere :: ContactWhere
defaultContactWhere =
  ContactWhere {
    organization: []
  , labTeamDepts: []
  , role: Nothing
  , office: Nothing
  , country: Nothing
  , city: Nothing
  , touch: Nothing
  , entry: Nothing
  , exit: Nothing
  }

newtype ContactTouch =
  ContactTouch
  { mail  :: Maybe String
  , phone :: Maybe String
  , url   :: Maybe String }

derive instance Newtype ContactTouch _
derive instance Generic ContactTouch _
instance Eq ContactTouch where eq = genericEq
derive newtype instance JSON.ReadForeign ContactTouch
derive newtype instance JSON.WriteForeign ContactTouch

defaultContactTouch :: ContactTouch
defaultContactTouch =
  ContactTouch {
    mail: Nothing
  , phone: Nothing
  , url: Nothing
  }


type HyperdataContactT =
  ( bdd            :: Maybe String
  , lastValidation :: Maybe String
  , source         :: Maybe String
  , title          :: Maybe String
  , uniqId         :: Maybe String
  , uniqIdBdd      :: Maybe String
  , who            :: Maybe ContactWho
  )
newtype HyperdataContact =
  HyperdataContact { ou             :: Array ContactWhere
                   | HyperdataContactT
                   }
derive instance Newtype HyperdataContact _
derive instance Generic HyperdataContact _
instance Eq HyperdataContact where eq = genericEq
instance JSON.ReadForeign HyperdataContact where
  readImpl f = do
    inst :: { where :: Maybe (Array ContactWhere) | HyperdataContactT } <- JSON.readImpl f

    pure $ HyperdataContact { bdd: inst.bdd
                            , lastValidation: inst.lastValidation
                            , ou: fromMaybe [] inst.where
                            , source: inst.source
                            , title: inst.title
                            , uniqId: inst.uniqId
                            , uniqIdBdd: inst.uniqIdBdd
                            , who: inst.who }
instance JSON.WriteForeign HyperdataContact
  where
    writeImpl (HyperdataContact hc) = JSON.writeImpl { bdd: hc.bdd
                                                     , lastValidation: hc.lastValidation
                                                     , where: hc.ou
                                                     , source: hc.source
                                                     , title: hc.title
                                                     , uniqId: hc.uniqId
                                                     , uniqIdBdd: hc.uniqIdBdd
                                                     , who: hc.who }

defaultHyperdataContact :: HyperdataContact
defaultHyperdataContact =
  HyperdataContact { bdd: Nothing
                   , who: Nothing
                   , ou: []
                   , title: Nothing
                   , source: Nothing
                   , lastValidation: Nothing
                   , uniqId: Nothing
                   , uniqIdBdd: Nothing
                   }

newtype HyperdataUser =
  HyperdataUser {
    shared :: Maybe HyperdataContact
  }
derive instance Newtype HyperdataUser _
derive instance Generic HyperdataUser _
instance Eq HyperdataUser where eq = genericEq
derive newtype instance JSON.ReadForeign HyperdataUser
derive newtype instance JSON.WriteForeign HyperdataUser

defaultHyperdataUser :: HyperdataUser
defaultHyperdataUser =
  HyperdataUser {
    shared: Just defaultHyperdataContact
  }


-- newtype HyperData c s =
--   HyperData
--   { common :: c
--   , shared :: s
--   , specific :: Map String String
--   }

-- instance (DecodeJson c, DecodeJson s) =>
--                                 DecodeJson (HyperData c s) where
--   decodeJson json = do
--     common <- decodeJson json
--     shared <- decodeJson json
--     specific <- decodeJson json
--     pure $ HyperData {common, shared, specific}

type ContactData = {contactNode :: Contact, defaultListId :: Int}
type ContactData' = {contactNode :: Contact', defaultListId :: Int}

_shared :: Lens' HyperdataUser HyperdataContact
_shared = lens getter setter
  where
    getter (HyperdataUser {shared}) = fromMaybe defaultHyperdataContact shared
    setter (HyperdataUser h) val = HyperdataUser $ h { shared = Just val }

_who :: Lens' HyperdataContact ContactWho
_who = lens getter setter
  where
    getter (HyperdataContact {who}) = fromMaybe defaultContactWho who
    setter (HyperdataContact hc) val = HyperdataContact $ hc { who = Just val }
_ouFirst :: Lens' HyperdataContact ContactWhere
_ouFirst = lens getter setter
  where
    getter (HyperdataContact {ou}) = fromMaybe defaultContactWhere $ A.head ou
    setter (HyperdataContact hc@{ou}) val = HyperdataContact $ hc { ou = fromMaybe [val] $ A.updateAt 0 val ou }

_lastName :: Lens' ContactWho String
_lastName = lens getter setter
  where
    getter (ContactWho {lastName}) = fromMaybe "" lastName
    setter (ContactWho cw) val = ContactWho $ cw { lastName = Just val }
_firstName :: Lens' ContactWho String
_firstName = lens getter setter
  where
    getter (ContactWho {firstName}) = fromMaybe "" firstName
    setter (ContactWho cw) val = ContactWho $ cw { firstName = Just val }

_organizationJoinComma :: Lens' ContactWhere String
_organizationJoinComma = lens getter setter
  where
    getter (ContactWhere {organization}) = S.joinWith pattern organization
    setter (ContactWhere cw) val = ContactWhere $ cw { organization = S.split (S.Pattern pattern) val }
    pattern = ", "

_labTeamDeptsJoinComma :: Lens' ContactWhere String
_labTeamDeptsJoinComma = lens getter setter
  where
    getter (ContactWhere {labTeamDepts}) = S.joinWith pattern labTeamDepts
    setter (ContactWhere cw) val = ContactWhere $ cw { labTeamDepts = S.split (S.Pattern pattern) val }
    pattern = ", "
_office :: Lens' ContactWhere String
_office = lens getter setter
  where
    getter (ContactWhere {office}) = fromMaybe "" office
    setter (ContactWhere cw) val = ContactWhere $ cw { office = Just val }
_city :: Lens' ContactWhere String
_city = lens getter setter
  where
    getter (ContactWhere {city}) = fromMaybe "" city
    setter (ContactWhere cw) val = ContactWhere $ cw { city = Just val }
_country :: Lens' ContactWhere String
_country = lens getter setter
  where
    getter (ContactWhere {country}) = fromMaybe "" country
    setter (ContactWhere cw) val = ContactWhere $ cw { country = Just val }
_role :: Lens' ContactWhere String
_role = lens getter setter
  where
    getter (ContactWhere {role}) = fromMaybe "" role
    setter (ContactWhere cw) val = ContactWhere $ cw { role = Just val }

_touch :: Lens' ContactWhere ContactTouch
_touch = lens getter setter
  where
    getter (ContactWhere {touch}) = fromMaybe defaultContactTouch touch
    setter (ContactWhere cw) val = ContactWhere $ cw { touch = Just val }
_mail :: Lens' ContactTouch String
_mail = lens getter setter
  where
    getter (ContactTouch {mail}) = fromMaybe "" mail
    setter (ContactTouch ct) val = ContactTouch $ ct { mail = Just val }
_phone :: Lens' ContactTouch String
_phone = lens getter setter
  where
    getter (ContactTouch {phone}) = fromMaybe "" phone
    setter (ContactTouch ct) val = ContactTouch $ ct { phone = Just val }

user_idP = Proxy :: Proxy "user_id"
userIdP = Proxy :: Proxy "userId"
parent_idP = Proxy :: Proxy "parent_id"
parentIdP = Proxy :: Proxy "parentId"
