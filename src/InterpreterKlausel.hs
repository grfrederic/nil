module InterpreterKlausel where

import qualified Data.Map as M
import Control.Monad.State.Lazy (get, put)

import Types
import InterpreterCheckDefined (checkDefinedKL)
import InterpreterCheckTypes (checkTypesKL)
import InterpreterSLD (runSLD)


-- updates EnvState with new Klausel
runKL :: Klausel -> IS [String]
runKL k = do
  checkDefinedKL k
  checkTypesKL k
  case k of
    Klausel (Konsequenz Nothing) (Bedingung ts) -> runSLD ts
    _ -> addKL k


-- new information
addKL :: Klausel -> IS [String]
addKL (Klausel (Konsequenz (Just (Cut _))) _) = undefined
addKL k@(Klausel (Konsequenz (Just (TermR r _))) _) = do
  s <- get
  let ke = klauselEnv s
  put $ s {klauselEnv = M.insertWith (flip (++)) r [k] ke}
  return []
addKL _ = undefined
