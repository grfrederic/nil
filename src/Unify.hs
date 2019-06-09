module Unify where

import qualified Data.Map as M

import Types


-- unification always performs occur check!


data Step = Substitute Substitution | Expand [(Dasein, Dasein)] | Fail

unificationStep :: (Dasein, Dasein) -> Step
unificationStep (d1, d2) | d1 == d2 = Substitute mempty 

-- variables
unificationStep (DaseinV x, d)
  | occurCheck x d = Substitute $ Substitution $ M.fromList [(x, d)] 
  | otherwise      = Fail
unificationStep (d, DaseinV x) 
  | occurCheck x d = Substitute $ Substitution $ M.fromList [(x, d)] 
  | otherwise      = Fail

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


occurCheck :: Variable -> Dasein -> Bool
occurCheck v (DaseinV w) = v /= w
occurCheck v (DaseinK _ ds) = all (occurCheck v) ds
