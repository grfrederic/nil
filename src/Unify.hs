module Unify where

import qualified Data.Map as M

import Types


substitute :: Substitution -> Dasein -> Dasein
substitute (Substitution m) (DaseinV v) | v `M.member` m = m M.! v
                                        | otherwise      = DaseinV v
substitute s (DaseinK k ds) = DaseinK k (map (substitute s) ds)


instance Monoid Substitution where
  mempty = Substitution M.empty
  mappend (Substitution m1) (Substitution m2) = Substitution $ M.union cm rm
    where
      cm = M.map (substitute $ Substitution m2) m1
      rm = M.difference m2 m1


data Step = Substitute Substitution | Expand [(Dasein, Dasein)] | Fail

unificationStep :: (Dasein, Dasein) -> Step
unificationStep (d1, d2) | d1 == d2 = Substitute mempty 

-- variables
unificationStep (DaseinV x, d) = Substitute $ Substitution $ M.fromList [(x, d)] 
unificationStep (d, DaseinV x) = Substitute $ Substitution $ M.fromList [(x, d)] 

-- constructors
unificationStep (DaseinK k ds1, DaseinK l ds2) | k == l = Expand $ zip ds1 ds2
unificationStep (DaseinK k _, DaseinK l _) | k /= l = Fail
unificationStep (_, _) = Fail


unify :: [(Dasein, Dasein)] -> Maybe Substitution
unify [] = Just mempty
unify (h:t) = case unificationStep h of
  Substitute s -> fmap (mappend s) (unify nt)
    where
      nt = map f t
      f (d1, d2) = (substitute s d1, substitute s d2)
  Expand ps -> unify $ ps ++ t
  Fail -> Nothing
