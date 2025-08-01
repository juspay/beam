-- | This module contains support for defining "ad-hoc" queries. That
-- is queries on tables that do not necessarily have corresponding
-- 'Beamable' table types.
module Database.Beam.Query.Adhoc
  ( Adhoc(..)

  , NamedField
  , table_, field_
  ) where

import Database.Beam.Query.Internal
import Database.Beam.Backend.SQL

import Control.Monad.Free.Church

import qualified Data.Text as T

class Adhoc structure where
  type AdhocTable structure (f :: * -> *) :: *

  mkAdhocField :: (forall a. T.Text -> f a) -> structure -> AdhocTable structure f

newtype NamedField a = NamedField T.Text

instance Adhoc (NamedField a) where
  type AdhocTable (NamedField a) f = f a

  mkAdhocField mk (NamedField nm) = mk nm

instance (Adhoc a, Adhoc b) => Adhoc (a, b) where
  type AdhocTable (a, b) y = (AdhocTable a y, AdhocTable b y)
  mkAdhocField mk (a, b) = (mkAdhocField mk a, mkAdhocField mk b)

instance (Adhoc a, Adhoc b, Adhoc c) => Adhoc (a, b, c) where
  type AdhocTable (a, b, c) y = (AdhocTable a y, AdhocTable b y, AdhocTable c y)
  mkAdhocField mk (a, b, c) = (mkAdhocField mk a, mkAdhocField mk b, mkAdhocField mk c)

instance (Adhoc a, Adhoc b, Adhoc c, Adhoc d) => Adhoc (a, b, c, d) where
  type AdhocTable (a, b, c, d) y = (AdhocTable a y, AdhocTable b y, AdhocTable c y, AdhocTable d y)
  mkAdhocField mk (a, b, c, d) = (mkAdhocField mk a, mkAdhocField mk b, mkAdhocField mk c, mkAdhocField mk d)

instance (Adhoc a, Adhoc b, Adhoc c, Adhoc d, Adhoc e) => Adhoc (a, b, c, d, e) where
  type AdhocTable (a, b, c, d, e) y = ( AdhocTable a y, AdhocTable b y, AdhocTable c y, AdhocTable d y
                                      , AdhocTable e y )
  mkAdhocField mk (a, b, c, d, e) = (mkAdhocField mk a, mkAdhocField mk b, mkAdhocField mk c, mkAdhocField mk d, mkAdhocField mk e)

instance (Adhoc a, Adhoc b, Adhoc c, Adhoc d, Adhoc e, Adhoc f) => Adhoc (a, b, c, d, e, f) where
  type AdhocTable (a, b, c, d, e, f) y = ( AdhocTable a y, AdhocTable b y, AdhocTable c y, AdhocTable d y
                                         , AdhocTable e y, AdhocTable f y )
  mkAdhocField mk (a, b, c, d, e, f) = (mkAdhocField mk a, mkAdhocField mk b, mkAdhocField mk c, mkAdhocField mk d, mkAdhocField mk e, mkAdhocField mk f)

instance (Adhoc a, Adhoc b, Adhoc c, Adhoc d, Adhoc e, Adhoc f, Adhoc g) => Adhoc (a, b, c, d, e, f, g) where
  type AdhocTable (a, b, c, d, e, f, g) y = ( AdhocTable a y, AdhocTable b y, AdhocTable c y, AdhocTable d y
                                            , AdhocTable e y, AdhocTable f y, AdhocTable g y )
  mkAdhocField mk (a, b, c, d, e, f, g) = (mkAdhocField mk a, mkAdhocField mk b, mkAdhocField mk c, mkAdhocField mk d, mkAdhocField mk e, mkAdhocField mk f, mkAdhocField mk g)

instance (Adhoc a, Adhoc b, Adhoc c, Adhoc d, Adhoc e, Adhoc f, Adhoc g, Adhoc h) =>
  Adhoc (a, b, c, d, e, f, g, h) where
  type AdhocTable (a, b, c, d, e, f, g, h) y = ( AdhocTable a y, AdhocTable b y, AdhocTable c y, AdhocTable d y
                                               , AdhocTable e y, AdhocTable f y, AdhocTable g y, AdhocTable h y )
  mkAdhocField mk (a, b, c, d, e, f, g, h) = (mkAdhocField mk a, mkAdhocField mk b, mkAdhocField mk c, mkAdhocField mk d, mkAdhocField mk e, mkAdhocField mk f, mkAdhocField mk g, mkAdhocField mk h)
instance (Adhoc a, Adhoc b, Adhoc c, Adhoc d, Adhoc e, Adhoc f, Adhoc g, Adhoc h, Adhoc i) =>
  Adhoc (a, b, c, d, e, f, g, h, i) where
  type AdhocTable (a, b, c, d, e, f, g, h, i) y = ( AdhocTable a y, AdhocTable b y, AdhocTable c y, AdhocTable d y
                                                 , AdhocTable e y, AdhocTable f y, AdhocTable g y, AdhocTable h y
                                                 , AdhocTable i y )
  mkAdhocField mk (a, b, c, d, e, f, g, h, i) = (mkAdhocField mk a, mkAdhocField mk b, mkAdhocField mk c, mkAdhocField mk d, mkAdhocField mk e, mkAdhocField mk f, mkAdhocField mk g, mkAdhocField mk h, mkAdhocField mk i)

instance (Adhoc a, Adhoc b, Adhoc c, Adhoc d, Adhoc e, Adhoc f, Adhoc g, Adhoc h, Adhoc i, Adhoc j) =>
  Adhoc (a, b, c, d, e, f, g, h, i, j) where
  type AdhocTable (a, b, c, d, e, f, g, h, i, j) y = ( AdhocTable a y, AdhocTable b y, AdhocTable c y, AdhocTable d y
                                                    , AdhocTable e y, AdhocTable f y, AdhocTable g y, AdhocTable h y
                                                    , AdhocTable i y, AdhocTable j y )
  mkAdhocField mk (a, b, c, d, e, f, g, h, i, j) = (mkAdhocField mk a, mkAdhocField mk b, mkAdhocField mk c, mkAdhocField mk d, mkAdhocField mk e, mkAdhocField mk f, mkAdhocField mk g, mkAdhocField mk h, mkAdhocField mk i, mkAdhocField mk j)

instance (Adhoc a, Adhoc b, Adhoc c, Adhoc d, Adhoc e, Adhoc f, Adhoc g, Adhoc h, Adhoc i, Adhoc j, Adhoc k) =>
  Adhoc (a, b, c, d, e, f, g, h, i, j, k) where
  type AdhocTable (a, b, c, d, e, f, g, h, i, j, k) y = ( AdhocTable a y, AdhocTable b y, AdhocTable c y, AdhocTable d y
                                                       , AdhocTable e y, AdhocTable f y, AdhocTable g y, AdhocTable h y
                                                       , AdhocTable i y, AdhocTable j y, AdhocTable k y )
  mkAdhocField mk (a, b, c, d, e, f, g, h, i, j, k) = (mkAdhocField mk a, mkAdhocField mk b, mkAdhocField mk c, mkAdhocField mk d, mkAdhocField mk e, mkAdhocField mk f, mkAdhocField mk g, mkAdhocField mk h, mkAdhocField mk i, mkAdhocField mk j, mkAdhocField mk k)

instance (Adhoc a, Adhoc b, Adhoc c, Adhoc d, Adhoc e, Adhoc f, Adhoc g, Adhoc h, Adhoc i, Adhoc j, Adhoc k, Adhoc l) =>
  Adhoc (a, b, c, d, e, f, g, h, i, j, k, l) where
  type AdhocTable (a, b, c, d, e, f, g, h, i, j, k, l) y = ( AdhocTable a y, AdhocTable b y, AdhocTable c y, AdhocTable d y
                                                          , AdhocTable e y, AdhocTable f y, AdhocTable g y, AdhocTable h y
                                                          , AdhocTable i y, AdhocTable j y, AdhocTable k y, AdhocTable l y )
  mkAdhocField mk (a, b, c, d, e, f, g, h, i, j, k, l) = (mkAdhocField mk a, mkAdhocField mk b, mkAdhocField mk c, mkAdhocField mk d, mkAdhocField mk e, mkAdhocField mk f, mkAdhocField mk g, mkAdhocField mk h, mkAdhocField mk i, mkAdhocField mk j, mkAdhocField mk k, mkAdhocField mk l)

instance (Adhoc a, Adhoc b, Adhoc c, Adhoc d, Adhoc e, Adhoc f, Adhoc g, Adhoc h, Adhoc i, Adhoc j, Adhoc k, Adhoc l, Adhoc m) =>
  Adhoc (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  type AdhocTable (a, b, c, d, e, f, g, h, i, j, k, l, m) y = ( AdhocTable a y, AdhocTable b y, AdhocTable c y, AdhocTable d y
                                                             , AdhocTable e y, AdhocTable f y, AdhocTable g y, AdhocTable h y
                                                             , AdhocTable i y, AdhocTable j y, AdhocTable k y, AdhocTable l y
                                                             , AdhocTable m y )
  mkAdhocField mk (a, b, c, d, e, f, g, h, i, j, k, l, m) = (mkAdhocField mk a, mkAdhocField mk b, mkAdhocField mk c, mkAdhocField mk d, mkAdhocField mk e, mkAdhocField mk f, mkAdhocField mk g, mkAdhocField mk h, mkAdhocField mk i, mkAdhocField mk j, mkAdhocField mk k, mkAdhocField mk l, mkAdhocField mk m)

instance (Adhoc a, Adhoc b, Adhoc c, Adhoc d, Adhoc e, Adhoc f, Adhoc g, Adhoc h, Adhoc i, Adhoc j, Adhoc k, Adhoc l, Adhoc m, Adhoc n) =>
  Adhoc (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
  type AdhocTable (a, b, c, d, e, f, g, h, i, j, k, l, m, n) y = ( AdhocTable a y, AdhocTable b y, AdhocTable c y, AdhocTable d y
                                                                , AdhocTable e y, AdhocTable f y, AdhocTable g y, AdhocTable h y
                                                                , AdhocTable i y, AdhocTable j y, AdhocTable k y, AdhocTable l y
                                                                , AdhocTable m y, AdhocTable n y )
  mkAdhocField mk (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = (mkAdhocField mk a, mkAdhocField mk b, mkAdhocField mk c, mkAdhocField mk d, mkAdhocField mk e, mkAdhocField mk f, mkAdhocField mk g, mkAdhocField mk h, mkAdhocField mk i, mkAdhocField mk j, mkAdhocField mk k, mkAdhocField mk l, mkAdhocField mk m, mkAdhocField mk n)

instance (Adhoc a, Adhoc b, Adhoc c, Adhoc d, Adhoc e, Adhoc f, Adhoc g, Adhoc h, Adhoc i, Adhoc j, Adhoc k, Adhoc l, Adhoc m, Adhoc n, Adhoc o) =>
  Adhoc (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  type AdhocTable (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) y = ( AdhocTable a y, AdhocTable b y, AdhocTable c y, AdhocTable d y
                                                                    , AdhocTable e y, AdhocTable f y, AdhocTable g y, AdhocTable h y
                                                                    , AdhocTable i y, AdhocTable j y, AdhocTable k y, AdhocTable l y
                                                                    , AdhocTable m y, AdhocTable n y, AdhocTable o y )
  mkAdhocField mk (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = (mkAdhocField mk a, mkAdhocField mk b, mkAdhocField mk c, mkAdhocField mk d, mkAdhocField mk e, mkAdhocField mk f, mkAdhocField mk g, mkAdhocField mk h, mkAdhocField mk i, mkAdhocField mk j, mkAdhocField mk k, mkAdhocField mk l, mkAdhocField mk m, mkAdhocField mk n, mkAdhocField mk o)

-- | Introduce a table into a query without using the 'Beamable' and 'Database' machinery.
--
-- The first argument is the optional name of the schema the table is in and the second is the name
-- of the table to source from.
--
-- The third argument is a tuple (or any nesting of tuples) where each value is of type 'NamedField'
-- (use 'field_' to construct).
--
-- The return value is a tuple (or any nesting of tuples) of the same shape as @structure@ but where
-- each value is a 'QExpr'.
--
-- For example, to source from the table @Table1@, with fields @Field1@ (A boolean), @Field2@ (a
-- timestamp), and @Field3@ (a string)
--
-- > table_ Nothing "Table1" ( field_ @Bool "Field1", field_ @UTCTime "Field2", field_ @Text "Field3" )
--
table_ :: forall be db structure s
        . (Adhoc structure, BeamSqlBackend be, Projectible be (AdhocTable structure (QExpr be s)))
       => Maybe T.Text -> T.Text -> structure -> Q be db s (AdhocTable structure (QExpr be s))
table_ schemaNm tblNm tbl =
  Q $ liftF (QAll (\_ -> fromTable (tableNamed (tableName schemaNm tblNm)) . Just . (, Nothing))
                  (\tblNm' -> let mk :: forall a. T.Text -> QExpr be s a
                                  mk nm = QExpr (\_ -> fieldE (qualifiedField tblNm' nm))
                              in mkAdhocField mk tbl)
                  (\_ -> Nothing) snd)

-- | Used to construct 'NamedField's, most often with an explicitly applied type.
--
-- The type can be omitted if the value is used unambiguously elsewhere.
field_ :: forall a. T.Text -> NamedField a
field_ = NamedField
