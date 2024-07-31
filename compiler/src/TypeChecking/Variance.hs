module TypeChecking.Variance
  ( Variance (..),
    (~*),
  )
where

data Variance = Bivariant | Covariant | Contravariant | Invariant deriving (Eq, Ord, Show)

instance Semigroup Variance where
  Invariant <> _ = Invariant
  _ <> Invariant = Invariant
  Covariant <> Contravariant = Invariant
  Contravariant <> Covariant = Invariant
  Covariant <> Covariant = Covariant
  Contravariant <> Contravariant = Contravariant
  v <> Bivariant = v
  Bivariant <> v = v

instance Monoid Variance where
  mempty = Invariant

(~*) :: Variance -> Variance -> Variance
Bivariant ~* _ = Bivariant
_ ~* Bivariant = Bivariant
Invariant ~* _ = Invariant
_ ~* Invariant = Invariant
Covariant ~* Covariant = Covariant
Covariant ~* Contravariant = Contravariant
Contravariant ~* Covariant = Contravariant
Contravariant ~* Contravariant = Covariant