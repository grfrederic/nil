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
      extractVarsT Cut = []
      extractVarsT (TermR _ ds) = ds >>= extractVarsD

      extractVarsD :: Dasein -> [Variable]
      extractVarsD (DaseinV v) = [v]
      extractVarsD (DaseinK _ ds) = ds >>= extractVarsD


data StepResult = NewGoals [Goal]
                | Found Substitution
                | StartCut Goal


getSolutions :: [Goal] -> IS [Substitution]
getSolutions [] = return []
getSolutions (g:t) = do
  step <- runStep g
  case step of
    NewGoals n -> getSolutions $ n ++ t
    Found s -> (s:) <$> getSolutions t
    StartCut ng -> getSolutions $ ng:cutTillCheckPoint t


cutTillCheckPoint :: [Goal] -> [Goal]
cutTillCheckPoint [] = []
cutTillCheckPoint (Checkpoint:t) = Checkpoint:t
cutTillCheckPoint (_:t) = cutTillCheckPoint t

runStep :: Goal -> IS StepResult

-- returning from klausel search
runStep Checkpoint = return $ NewGoals []

-- found solution
runStep Goal{terms = [], solution = sol} = return $ Found sol

-- new term
runStep goal@Goal{era = e, terms = TermR r ds:t} = do
  ke <- klauselEnv <$> get
  let ks = concat $ maybeToList $ M.lookup r ke
  let tes = mapMaybe (tryUnify e (TermR r ds)) ks
  return $ NewGoals $ map updateGoal tes <*> [goal {terms = t}]

-- cut
runStep goal@Goal{era = e, terms = Cut:t} =
  return $ StartCut $ goal {era = e+1, terms = t}


tryUnify :: Integer -> Term -> Klausel -> Maybe (Substitution, [Term])
tryUnify _ _ (Klausel (Konsequenz Nothing) _) = undefined
tryUnify e t1 (Klausel (Konsequenz (Just t2)) (Bedingung ts)) =
  case unify $ zip ds1 ds3 of
    Just sub -> Just (sub, map termMangle ts)
      where
        termMangle :: Term -> Term
        termMangle Cut = Cut
        termMangle (TermR r ds) = TermR r (map (eraMangle e) ds)
    Nothing -> Nothing
    where
      (TermR _ ds1) = t1
      (TermR _ ds2) = t2
      ds3 = map (eraMangle e) ds2


updateGoal :: (Substitution, [Term]) -> Goal -> Goal
updateGoal (s, ts) goal = Goal {era = ne, solution = ns, terms = nt}
  where
    ne = 1 + era goal
    ns = mappend (solution goal) s
    nt = map update $ mappend ts (terms goal)
    update :: Term -> Term
    update (TermR r ds) = TermR r (map (substitute s) ds)
    update Cut = Cut


eraMangle :: Integer -> Dasein -> Dasein
eraMangle e (DaseinK k ds) = DaseinK k (map (eraMangle e) ds)
eraMangle e (DaseinV (Variable n)) = DaseinV $ Variable nn
  where
    nn = "-era_" ++ show e ++ "_" ++ n
