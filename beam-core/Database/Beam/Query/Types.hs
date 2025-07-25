{-# LANGUAGE UndecidableInstances #-}
module Database.Beam.Query.Types
  ( Q, QExpr, QGenExpr(..), QExprToIdentity, QExprToField, QWindow

  , Projectible

  , HasQBuilder(..) ) where

import Database.Beam.Query.Internal
import Database.Beam.Query.SQL92

import Database.Beam.Schema.Tables

import Database.Beam.Backend.SQL
-- import Database.Beam.Backend.SQL.Builder
-- import Database.Beam.Backend.SQL.AST

import Control.Monad.Identity
import Data.Vector.Sized (Vector)

type family QExprToIdentity x
type instance QExprToIdentity (table (QGenExpr context syntax s)) = table Identity
type instance QExprToIdentity (table (Nullable c)) = Maybe (QExprToIdentity (table c))
type instance QExprToIdentity (QGenExpr context syntax s a) = a
type instance QExprToIdentity ()     = ()
type instance QExprToIdentity (a, b) = (QExprToIdentity a, QExprToIdentity b)
type instance QExprToIdentity (a, b, c) = (QExprToIdentity a, QExprToIdentity b, QExprToIdentity c)
type instance QExprToIdentity (a, b, c, d) = (QExprToIdentity a, QExprToIdentity b, QExprToIdentity c, QExprToIdentity d)
type instance QExprToIdentity (a, b, c, d, e) = (QExprToIdentity a, QExprToIdentity b, QExprToIdentity c, QExprToIdentity d, QExprToIdentity e)
type instance QExprToIdentity (a, b, c, d, e, f) = (QExprToIdentity a, QExprToIdentity b, QExprToIdentity c, QExprToIdentity d, QExprToIdentity e, QExprToIdentity f)
type instance QExprToIdentity (a, b, c, d, e, f, g) =
  ( QExprToIdentity a, QExprToIdentity b, QExprToIdentity c, QExprToIdentity d, QExprToIdentity e, QExprToIdentity f
  , QExprToIdentity g)
type instance QExprToIdentity (a, b, c, d, e, f, g, h) =
  ( QExprToIdentity a, QExprToIdentity b, QExprToIdentity c, QExprToIdentity d, QExprToIdentity e, QExprToIdentity f
  , QExprToIdentity g, QExprToIdentity h )
type instance QExprToIdentity (a, b, c, d, e, f, g, h, i) =
  ( QExprToIdentity a, QExprToIdentity b, QExprToIdentity c, QExprToIdentity d, QExprToIdentity e, QExprToIdentity f
  , QExprToIdentity g, QExprToIdentity h, QExprToIdentity i)
type instance QExprToIdentity (a, b, c, d, e, f, g, h, i, j) =
  ( QExprToIdentity a, QExprToIdentity b, QExprToIdentity c, QExprToIdentity d, QExprToIdentity e, QExprToIdentity f
  , QExprToIdentity g, QExprToIdentity h, QExprToIdentity i, QExprToIdentity j)
type instance QExprToIdentity (a, b, c, d, e, f, g, h, i, j, k) =
  ( QExprToIdentity a, QExprToIdentity b, QExprToIdentity c, QExprToIdentity d, QExprToIdentity e, QExprToIdentity f
  , QExprToIdentity g, QExprToIdentity h, QExprToIdentity i, QExprToIdentity j, QExprToIdentity k)
type instance QExprToIdentity (a, b, c, d, e, f, g, h, i, j, k, l) =
  ( QExprToIdentity a, QExprToIdentity b, QExprToIdentity c, QExprToIdentity d, QExprToIdentity e, QExprToIdentity f
  , QExprToIdentity g, QExprToIdentity h, QExprToIdentity i, QExprToIdentity j, QExprToIdentity k, QExprToIdentity l)
type instance QExprToIdentity (a, b, c, d, e, f, g, h, i, j, k, l, m) =
  ( QExprToIdentity a, QExprToIdentity b, QExprToIdentity c, QExprToIdentity d, QExprToIdentity e, QExprToIdentity f
  , QExprToIdentity g, QExprToIdentity h, QExprToIdentity i, QExprToIdentity j, QExprToIdentity k, QExprToIdentity l
  , QExprToIdentity m)
type instance QExprToIdentity (a, b, c, d, e, f, g, h, i, j, k, l, m, n) =
  ( QExprToIdentity a, QExprToIdentity b, QExprToIdentity c, QExprToIdentity d, QExprToIdentity e, QExprToIdentity f
  , QExprToIdentity g, QExprToIdentity h, QExprToIdentity i, QExprToIdentity j, QExprToIdentity k, QExprToIdentity l
  , QExprToIdentity m, QExprToIdentity n)
type instance QExprToIdentity (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) =
  ( QExprToIdentity a, QExprToIdentity b, QExprToIdentity c, QExprToIdentity d, QExprToIdentity e, QExprToIdentity f
  , QExprToIdentity g, QExprToIdentity h, QExprToIdentity i, QExprToIdentity j, QExprToIdentity k, QExprToIdentity l
  , QExprToIdentity m, QExprToIdentity n, QExprToIdentity o)
type instance QExprToIdentity (Vector n a) = Vector n (QExprToIdentity a)

-- TODO can this be unified with QExprToIdentity?
type family QExprToField x
type instance QExprToField (table (QGenExpr context syntax s)) = table (QField s)
type instance QExprToField (table (Nullable (QGenExpr context syntax s))) = table (Nullable (QField s))
type instance QExprToField (QGenExpr ctxt syntax s a) = QField s a
type instance QExprToField () = ()
type instance QExprToField (a, b) = (QExprToField a, QExprToField b)
type instance QExprToField (a, b, c) = (QExprToField a, QExprToField b, QExprToField c)
type instance QExprToField (a, b, c, d) = (QExprToField a, QExprToField b, QExprToField c, QExprToField d)
type instance QExprToField (a, b, c, d, e) = (QExprToField a, QExprToField b, QExprToField c, QExprToField d, QExprToField e)
type instance QExprToField (a, b, c, d, e, f) =
  ( QExprToField a, QExprToField b, QExprToField c, QExprToField d
  , QExprToField e, QExprToField f )
type instance QExprToField (a, b, c, d, e, f, g) =
  ( QExprToField a, QExprToField b, QExprToField c, QExprToField d
  , QExprToField e, QExprToField f, QExprToField g )
type instance QExprToField (a, b, c, d, e, f, g, h) =
  ( QExprToField a, QExprToField b, QExprToField c, QExprToField d
  , QExprToField e, QExprToField f, QExprToField g, QExprToField h)
type instance QExprToField (a, b, c, d, e, f, g, h, i) =
  ( QExprToField a, QExprToField b, QExprToField c, QExprToField d
  , QExprToField e, QExprToField f, QExprToField g, QExprToField h, QExprToField i)
type instance QExprToField (a, b, c, d, e, f, g, h, i, j) =
  ( QExprToField a, QExprToField b, QExprToField c, QExprToField d
  , QExprToField e, QExprToField f, QExprToField g, QExprToField h, QExprToField i, QExprToField j)
type instance QExprToField (a, b, c, d, e, f, g, h, i, j, k) =
  ( QExprToField a, QExprToField b, QExprToField c, QExprToField d
  , QExprToField e, QExprToField f, QExprToField g, QExprToField h, QExprToField i, QExprToField j, QExprToField k)
type instance QExprToField (a, b, c, d, e, f, g, h, i, j, k, l) =
  ( QExprToField a, QExprToField b, QExprToField c, QExprToField d
  , QExprToField e, QExprToField f, QExprToField g, QExprToField h, QExprToField i, QExprToField j, QExprToField k, QExprToField l)
type instance QExprToField (a, b, c, d, e, f, g, h, i, j, k, l, m) =
  ( QExprToField a, QExprToField b, QExprToField c, QExprToField d
  , QExprToField e, QExprToField f, QExprToField g, QExprToField h, QExprToField i, QExprToField j, QExprToField k, QExprToField l, QExprToField m)
type instance QExprToField (a, b, c, d, e, f, g, h, i, j, k, l, m, n) =
  ( QExprToField a, QExprToField b, QExprToField c, QExprToField d
  , QExprToField e, QExprToField f, QExprToField g, QExprToField h, QExprToField i, QExprToField j, QExprToField k, QExprToField l, QExprToField m, QExprToField n)
type instance QExprToField (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) =
  ( QExprToField a, QExprToField b, QExprToField c, QExprToField d
  , QExprToField e, QExprToField f, QExprToField g, QExprToField h, QExprToField i, QExprToField j, QExprToField k, QExprToField l, QExprToField m, QExprToField n, QExprToField o)
type instance QExprToField (Vector n a) = Vector n (QExprToField a)

class BeamSqlBackend be => HasQBuilder be where
  buildSqlQuery :: Projectible be a
                => TablePrefix -> Q be db s a -> BeamSqlBackendSelectSyntax be
instance BeamSqlBackend (MockSqlBackend cmd) => HasQBuilder (MockSqlBackend cmd) where
  buildSqlQuery = buildSql92Query' True
-- instance HasQBuilder Select where
--   buildSqlQuery = buildSql92Query' True
