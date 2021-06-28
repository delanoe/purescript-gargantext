module Gargantext.Components.Nodes.Annuaire.User.Contacts.Types where

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, (.:), (.:!), (.:?), (:=), (~>), jsonEmptyObject)
import Data.Array as A
import Data.Generic.Rep (class Generic)
import Data.Eq.Generic (genericEq)
import Data.Lens
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.String as S

import Gargantext.Prelude

import Gargantext.Utils.DecodeMaybe ((.?|))

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
instance Eq NodeContact where
  eq = genericEq
instance DecodeJson NodeContact where
  decodeJson json = do
    obj       <- decodeJson json
    date      <- obj .?| "date"
    hyperdata <- obj .: "hyperdata"
    id        <- obj .: "id"
    name      <- obj .:! "name"
    parentId  <- obj .?| "parent_id"
    typename  <- obj .?| "typename"
    userId    <- obj .:! "user_id"

    pure $ NodeContact { id
                   , date
                   , hyperdata
                   , name
                   , parentId
                   , typename
                   , userId
                   }
derive instance Newtype NodeContact _

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
instance Eq Contact' where
  eq = genericEq
instance DecodeJson Contact' where
  decodeJson json = do
    obj       <- decodeJson json
    date      <- obj .?| "date"
    hyperdata <- obj .: "hyperdata"
    id        <- obj .: "id"
    name      <- obj .:! "name"
    parentId  <- obj .?| "parent_id"
    typename  <- obj .?| "typename"
    userId    <- obj .:! "user_id"

    pure $ Contact' { id
                   , date
                   , hyperdata
                   , name
                   , parentId
                   , typename
                   , userId
                   }


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
instance Eq Contact where
  eq = genericEq
instance DecodeJson Contact where
  decodeJson json = do
    obj       <- decodeJson json
    date      <- obj .?| "date"
    hyperdata <- obj .: "hyperdata"
    id        <- obj .: "id"
    name      <- obj .:! "name"
    parentId  <- obj .?| "parent_id"
    typename  <- obj .?| "typename"
    userId    <- obj .:! "user_id"

    pure $ Contact { id
                   , date
                   , hyperdata
                   , name
                   , parentId
                   , typename
                   , userId
                   }

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


instance DecodeJson User where
  decodeJson json = do
    obj       <- decodeJson json
    date      <- obj .?| "date"
    hyperdata <- obj .: "hyperdata"
    id        <- obj .: "id"
    name      <- obj .:! "name"
    parentId  <- obj .?| "parent_id"
    typename  <- obj .?| "typename"
    userId    <- obj .:! "user_id"

    pure $ User { id
                , date
                , hyperdata
                , name
                , parentId
                , typename
                , userId
                }


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
instance Eq ContactWho where
  eq = genericEq
instance DecodeJson ContactWho
  where
    decodeJson json = do
      obj <- decodeJson json
      idWho     <- obj .:? "id"
      firstName <- obj .:? "firstName"
      lastName  <- obj .:? "lastName"
      keywords  <- obj .:! "keywords"
      freetags  <- obj .:! "freetags"

      let k = fromMaybe [] keywords
      let f = fromMaybe [] freetags

      pure $ ContactWho {idWho, firstName, lastName, keywords:k, freetags:f}
instance EncodeJson ContactWho
  where
    encodeJson (ContactWho cw) =
         "id"        := cw.idWho
      ~> "firstName" := cw.firstName
      ~> "lastName"  := cw.lastName
      ~> "keywords"  := cw.keywords
      ~> "freetags"  := cw.freetags
      ~> jsonEmptyObject

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
instance Eq ContactWhere where
  eq = genericEq
instance DecodeJson ContactWhere
  where
    decodeJson json = do
      obj <- decodeJson json
      organization  <- obj .:! "organization"
      labTeamDepts  <- obj .:! "labTeamDepts"
      role          <- obj .:? "role"
      office        <- obj .:? "office"
      country       <- obj .:? "country"
      city          <- obj .:? "city"
      touch         <- obj .:? "touch"
      entry         <- obj .:? "entry"
      exit          <- obj .:? "exit"

      let o = fromMaybe [] organization
      let l = fromMaybe [] labTeamDepts

      pure $ ContactWhere {organization:o, labTeamDepts:l, role, office, country, city, touch, entry, exit}
instance EncodeJson ContactWhere
  where
    encodeJson (ContactWhere cw) =
         "city"         := cw.city
      ~> "country"      := cw.country
      ~> "entry"        := cw.entry
      ~> "exit"         := cw.exit
      ~> "labTeamDepts" := cw.labTeamDepts
      ~> "office"       := cw.office
      ~> "organization" := cw.organization
      ~> "role"         := cw.role
      ~> "touch"        := cw.touch
      ~> jsonEmptyObject

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
instance Eq ContactTouch where
  eq = genericEq
instance DecodeJson ContactTouch
  where
    decodeJson json = do
      obj <- decodeJson json
      mail  <- obj .:? "mail"
      phone <- obj .:? "phone"
      url   <- obj .:? "url"
      pure $ ContactTouch {mail, phone, url}
instance EncodeJson ContactTouch
  where
    encodeJson (ContactTouch ct) =
         "mail"  := ct.mail
      ~> "phone" := ct.phone
      ~> "url"   := ct.url
      ~> jsonEmptyObject

defaultContactTouch :: ContactTouch
defaultContactTouch =
  ContactTouch {
    mail: Nothing
  , phone: Nothing
  , url: Nothing
  }


newtype HyperdataContact =
     HyperdataContact { bdd            :: Maybe String
                      , lastValidation :: Maybe String
                      , ou             :: (Array ContactWhere)
                      , source         :: Maybe String
                      , title          :: Maybe String
                      , uniqId         :: Maybe String
                      , uniqIdBdd      :: Maybe String
                      , who            :: Maybe ContactWho
                      }
derive instance Newtype HyperdataContact _
derive instance Generic HyperdataContact _
instance Eq HyperdataContact where
  eq = genericEq
instance DecodeJson HyperdataContact
  where
    decodeJson json = do
      obj <- decodeJson json
      bdd            <- obj .:? "bdd"
      lastValidation <- obj .:? "lastValidation"
      ou             <- obj .:! "where"
      source         <- obj .:? "source"
      title          <- obj .:? "title"
      uniqId         <- obj .:? "uniqId"
      uniqIdBdd      <- obj .:? "uniqIdBdd"
      who            <- obj .:! "who"
      
      let ou' = fromMaybe [] ou

      pure $ HyperdataContact {bdd, who, ou:ou', title, source, lastValidation, uniqId, uniqIdBdd}
instance EncodeJson HyperdataContact
  where
    encodeJson (HyperdataContact {bdd, lastValidation, ou, source, title, uniqId, uniqIdBdd, who}) =
         "bdd" := bdd
      ~> "lastValidation" := lastValidation
      ~> "where" := ou
      ~> "source" := source
      ~> "title" := title
      ~> "uniqId" := uniqId
      ~> "uniqIdBdd" := uniqIdBdd
      ~> "who" := who
      ~> jsonEmptyObject

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
instance Eq HyperdataUser where
  eq = genericEq
instance DecodeJson HyperdataUser
  where
    decodeJson json = do
      obj    <- decodeJson json
      shared <- obj .:? "shared"
      pure $ HyperdataUser { shared }
instance EncodeJson HyperdataUser
  where
    encodeJson (HyperdataUser {shared}) =
         "shared" := shared
      ~> jsonEmptyObject

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
