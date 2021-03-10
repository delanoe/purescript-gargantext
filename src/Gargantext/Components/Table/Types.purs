module Gargantext.Components.Table.Types where

import Prelude (class Eq, class Show, (<>))
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe)
import Data.Sequence as Seq
import Reactix as R
import Gargantext.Components.Search (SearchType)
import Data.Generic.Rep (class Generic)

type Params = { limit      :: Int
              , offset     :: Int
              , orderBy    :: OrderBy
              , searchType :: SearchType
              }

type OrderBy = Maybe (OrderByDirection ColumnName)

data OrderByDirection a = ASC a | DESC a
derive instance genericOrderByDirection :: Generic (OrderByDirection a) _
instance showOrderByDirection :: Show a => Show (OrderByDirection a) where
  show = genericShow
derive instance eqOrderByDirection :: Eq a => Eq (OrderByDirection a)
orderByToForm :: OrderByDirection ColumnName -> String
orderByToForm (ASC  (ColumnName x)) = x <> "Asc"
orderByToForm (DESC (ColumnName x)) = x <> "Desc"

newtype ColumnName = ColumnName String
derive instance genericColumnName :: Generic ColumnName _
instance showColumnName :: Show ColumnName where
  show = genericShow
derive instance eqColumnName :: Eq ColumnName
columnName :: ColumnName -> String
columnName (ColumnName c) = c

type Props =
  ( syncResetButton  :: Array R.Element
  , colNames     :: Array ColumnName
  , container    :: Record TableContainerProps -> R.Element
  , params       :: R.State Params
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

