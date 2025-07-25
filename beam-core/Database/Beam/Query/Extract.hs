module Database.Beam.Query.Extract
    ( -- * SQL @EXTRACT@ support

      ExtractField(..),

      extract_,
      jsonValid_,
      unsafeJsonExtract_,
      jsonUnquote_,

      -- ** SQL92 fields
      hour_, minutes_, seconds_,
      year_, month_, day_,

      HasSqlTime, HasSqlDate
    ) where

import Database.Beam.Query.Internal ( QGenExpr(..) )

import Database.Beam.Backend.SQL ( BeamSqlBackend, BeamSqlBackendSyntax )
import Database.Beam.Backend.SQL.SQL92 ( Sql92ExtractFieldSyntax
                                       , IsSql92ExpressionSyntax(..)
                                       , IsSql92ExtractFieldSyntax(..) )

import Data.Time (LocalTime, UTCTime, TimeOfDay, Day)
import Data.Text (Text)
import           Control.Applicative (liftA2)

-- | A field that can be extracted from SQL expressions of type 'tgt'
-- that results in a type 'a', in backend 'be'.
newtype ExtractField be tgt a
    = ExtractField (Sql92ExtractFieldSyntax (BeamSqlBackendSyntax be))

-- | Extracts the given field from the target expression
extract_ :: BeamSqlBackend be
         => ExtractField be tgt a -> QGenExpr ctxt be s tgt -> QGenExpr cxt be s a
extract_ (ExtractField field) (QExpr expr) =
    QExpr (extractE field <$> expr)

jsonValid_ :: BeamSqlBackend be
        => QGenExpr ctxt be s a -> QGenExpr ctxt be s Bool
jsonValid_ (QExpr expr) = QExpr (jsonValidE <$> expr)

unsafeJsonExtract_ :: BeamSqlBackend be 
             => QGenExpr ctxt be s a -> QGenExpr ctxt be s Text -> QGenExpr ctxt be s b
unsafeJsonExtract_ (QExpr expr) (QExpr field) =
    QExpr (liftA2 jsonExtractE expr field)

jsonUnquote_ :: BeamSqlBackend be
             => QGenExpr ctxt be s a -> QGenExpr ctxt be s b
jsonUnquote_ (QExpr expr) = QExpr (jsonUnquoteE <$> expr)

-- | Type-class for types that contain a time component
class HasSqlTime tgt
instance HasSqlTime LocalTime
instance HasSqlTime UTCTime
instance HasSqlTime TimeOfDay

-- | Extracts the hours, minutes, or seconds from any timestamp or
-- time field
hour_, minutes_, seconds_
    :: ( BeamSqlBackend be, HasSqlTime tgt )
    => ExtractField be tgt Double
hour_    = ExtractField hourField
minutes_ = ExtractField minutesField
seconds_ = ExtractField secondsField

-- | Type-class for types that contain a date component
class HasSqlDate tgt
instance HasSqlDate LocalTime
instance HasSqlDate UTCTime
instance HasSqlDate Day

year_, month_, day_
    :: ( BeamSqlBackend be, HasSqlDate tgt )
    => ExtractField be tgt Double
year_  = ExtractField yearField
month_ = ExtractField monthField
day_   = ExtractField dayField
