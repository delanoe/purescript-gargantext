module Gargantext.Components.Table.Types where

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Sequence as Seq
import Data.Show.Generic (genericShow)
import Gargantext.Components.Search (SearchType)
import Gargantext.Types as GT
import Prelude (class Eq, class Show, (<>))
import Reactix as R
import Toestand as T

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

orderByToGTOrderBy :: OrderBy -> Maybe GT.OrderBy
orderByToGTOrderBy (Just (ASC (ColumnName "Date"))) = Just GT.DateAsc
orderByToGTOrderBy (Just (DESC (ColumnName "Date"))) = Just GT.DateDesc
orderByToGTOrderBy (Just (ASC (ColumnName "Title"))) = Just GT.TitleAsc
orderByToGTOrderBy (Just (DESC (ColumnName "Title"))) = Just GT.TitleDesc
orderByToGTOrderBy (Just (ASC (ColumnName "Score"))) = Just GT.ScoreAsc
orderByToGTOrderBy (Just (DESC (ColumnName "Score"))) = Just GT.ScoreDesc
orderByToGTOrderBy (Just (ASC (ColumnName "Terms"))) = Just GT.TermAsc
orderByToGTOrderBy (Just (DESC (ColumnName "Terms"))) = Just GT.TermDesc
orderByToGTOrderBy (Just (ASC (ColumnName "Source"))) = Just GT.SourceAsc
orderByToGTOrderBy (Just (DESC (ColumnName "Source"))) = Just GT.SourceDesc
orderByToGTOrderBy (Just _) = Nothing
orderByToGTOrderBy Nothing = Nothing

newtype ColumnName = ColumnName String
derive instance Generic ColumnName _
instance Show ColumnName where
  show = genericShow
derive instance Eq ColumnName
columnName :: ColumnName -> String
columnName (ColumnName c) = c

type Props =
  ( syncResetButton   :: Array R.Element
  , colNames          :: Array ColumnName
  , container         :: Record TableContainerProps -> R.Element
  , params            :: T.Box Params
  , rows              :: Rows
  , totalRecords      :: Int
  , wrapColElts       :: ColumnName -> Array R.Element -> Array R.Element
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

