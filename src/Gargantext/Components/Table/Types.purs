module Gargantext.Components.Table.Types where

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Maybe (Maybe)
import Data.Sequence as Seq
import Reactix as R
import Toestand as T

import Prelude (class Eq, class Show, (<>))

import Gargantext.Components.Search (SearchType)

type Params = { limit      :: Int
              , offset     :: Int
              , orderBy    :: OrderBy
              , searchType :: SearchType
              }

type OrderBy = Maybe (OrderByDirection ColumnName)

data OrderByDirection a = ASC a | DESC a
derive instance Generic (OrderByDirection a) _
instance Show a => Show (OrderByDirection a) where
  show = genericShow
derive instance Eq a => Eq (OrderByDirection a)
orderByToForm :: OrderByDirection ColumnName -> String
orderByToForm (ASC  (ColumnName x)) = x <> "Asc"
orderByToForm (DESC (ColumnName x)) = x <> "Desc"

newtype ColumnName = ColumnName String
derive instance Generic ColumnName _
instance Show ColumnName where
  show = genericShow
derive instance Eq ColumnName
columnName :: ColumnName -> String
columnName (ColumnName c) = c

type Props =
  ( syncResetButton  :: Array R.Element
  , colNames     :: Array ColumnName
  , container    :: Record TableContainerProps -> R.Element
  , params       :: T.Box Params
  , rows         :: Rows
  , totalRecords :: Int
  , wrapColElts  :: ColumnName -> Array R.Element -> Array R.Element
                 -- ^ Use `const identity` as a default behavior.
  )
type TableContainerProps =
  ( syncResetButton         :: Array R.Element
  , pageSizeControl     :: R.Element
  , pageSizeDescription :: R.Element
  , paginationLinks     :: R.Element
  , tableHead           :: R.Element
  , tableBody           :: Array R.Element
  )

type Row = { row :: R.Element, delete :: Boolean }
type Rows = Seq.Seq Row

