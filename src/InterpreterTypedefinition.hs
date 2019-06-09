module InterpreterTypedefinition where

import qualified Data.Map as M
import Control.Monad.State.Lazy (get, put)

import Types
import PrettyShow


-- updates EnvState with new Typdefinition
runTD :: TypDefinition -> IS ()
runTD (KonstruktorTD k ts t) = do
  s <- get
  let ke = konstruktorEnv s
  if k `M.member` ke
     then throw $ Redefined (psK k)
     else put $ s {konstruktorEnv = M.insert k (t, ts) ke}

runTD (RecordTD k tts t) = do
  s <- get
  let ke = konstruktorEnv s
  let re = recordEnv s
  if k `M.member` ke
     then throw $ Redefined (psK k)
     else do
       let tags = map fst tts
       let typs = map snd tts
       put $ s { konstruktorEnv = M.insert k (t, typs) ke
               , recordEnv = M.insert k tags re
               }

runTD (RelationTD r ts) = do
  s <- get
  let re = relationEnv s
  if r `M.member` re
     then throw $ Redefined (psR r)
     else put $ s {relationEnv = M.insert r ts re}
