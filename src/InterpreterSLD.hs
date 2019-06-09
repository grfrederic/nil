module InterpreterSLD where

import qualified Data.Map as M
import Control.Monad.State.Lazy (get)
import Data.Maybe (mapMaybe, maybeToList)

import Types
import Unify
import PrettyShow
import InterpreterSugar (resugarSubstitution)


runSLD :: [Term] -> IS [String]
runSLD ts = do
  ss <- getSolutions [startGoal]
  let fs = map filterForStart ss
  rs <- mapM resugarSubstitution fs
  return $ map psS rs
    where
      startGoal = Goal {era = 0, solution = mempty, terms = ts}

      -- we don't need to show what other variables were bound to
      filterForStart :: Substitution -> Substitution
      filterForStart (Substitution m) = Substitution $ M.filterWithKey cr m
        where
          cr v _ = v `elem` startVars

      startVars = ts >>= extractVarsT

      extractVarsT :: Term -> [Variable]
      extractVarsT (Term _ ds) = ds >>= extractVarsD

      extractVarsD :: Dasein -> [Variable]
      extractVarsD (DaseinV v) = [v]
      extractVarsD (DaseinK _ ds) = ds >>= extractVarsD


getSolutions :: [Goal] -> IS [Substitution]
getSolutions [] = return []
getSolutions (g:t) = do
  step <- runStep g
  case step of
    Left n -> getSolutions $ n ++ t
    Right s -> (s:) <$> getSolutions t


runStep :: Goal -> IS (Either [Goal] Substitution)
runStep Goal{terms = [], solution = sol} = return $ Right sol
runStep goal@Goal{era = e, terms = Term r ds:t} = do
  ke <- klauselEnv <$> get
  let ks = concat $ maybeToList $ M.lookup r ke
  let tes = mapMaybe (tryUnify e (Term r ds)) ks
  return $ Left $ map updateGoal tes <*> [goal {terms = t}]


tryUnify :: Integer -> Term -> Klausel -> Maybe (Substitution, [Term])
tryUnify _ _ (Klausel (Konsequenz Nothing) _) = undefined
tryUnify e t1 (Klausel (Konsequenz (Just t2)) (Bedingung ts)) =
  case unify $ zip ds1 ds3 of
    Just sub -> Just (sub, map termMangle ts)
      where
        termMangle :: Term -> Term
        termMangle (Term r ds) = Term r (map (eraMangle e) ds)
    Nothing -> Nothing
    where
      (Term _ ds1) = t1
      (Term _ ds2) = t2
      ds3 = map (eraMangle e) ds2


updateGoal :: (Substitution, [Term]) -> Goal -> Goal
updateGoal (s, ts) goal = Goal {era = ne, solution = ns, terms = nt}
  where
    ne = 1 + era goal
    ns = mappend (solution goal) s
    nt = map update $ mappend ts (terms goal)
    update :: Term -> Term
    update (Term r ds) = Term r (map (substitute s) ds)


eraMangle :: Integer -> Dasein -> Dasein
eraMangle e (DaseinK k ds) = DaseinK k (map (eraMangle e) ds)
eraMangle e (DaseinV (Variable n)) = DaseinV $ Variable nn
  where
    nn = "-era_" ++ show e ++ "_" ++ n
