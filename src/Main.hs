import System.Environment
import qualified Control.Monad.Trans.State.Lazy as S
import qualified Control.Monad.Trans.Except as E
import qualified Data.Text as T
import qualified Text.Megaparsec as P

import Parse
import Types
import PrettyShow
import InterpreterTypedefinition
import InterpreterKlausel


-- run all lines and return outputs
runNil :: Program -> [String]
runNil (Program ls) =
  case S.evalState (E.runExceptT runPR) startEnvState of
    Right r -> r
    Left e -> [psE e]
    where
      runPL :: Either TypDefinition Klausel -> IS [String]
      runPL = either (\t -> runTD t >> return []) runKL

      runPR :: IS [String]
      runPR = concat <$> mapM runPL ls


main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      cont <- readFile file
      let parsed = P.runParser programParser file (T.pack cont)
      case parsed of
        Left err -> putStr $ P.errorBundlePretty err
        Right tree -> putStr $ unlines $ runNil tree
    _ -> putStrLn "Usage: nil source.nil"

