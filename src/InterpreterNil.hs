module InterpreterNil where

import qualified Control.Monad.Trans.State.Lazy as S
import qualified Control.Monad.Trans.Except as E

import Types
import PrettyShow
import InterpreterTypedefinition
import InterpreterKlausel
import InterpreterSugar


runNil :: ProgrammS -> [String]
runNil pS =
  case S.evalState (E.runExceptT (runProgramm pS)) startEnvState of
    Right r -> r
    Left e -> [psE e]


runProgramm :: ProgrammS -> IS [String]
runProgramm (ProgrammS ls) = concat <$> mapM runProgrammzeileS ls


runProgrammzeileS :: ProgrammzeileS -> IS [String]
runProgrammzeileS pzS = concat <$> (desugarProgrammzeile pzS >>= mapM runProgrammzeile) 


runProgrammzeile :: Programmzeile -> IS [String]
runProgrammzeile = either (\t -> runTD t >> return []) runKL
