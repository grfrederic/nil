import System.Environment
import qualified Data.Text as T
import qualified Text.Megaparsec as P

import Parse
import InterpreterNil


main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      cont <- readFile file
      let parsed = P.runParser programmParser file (T.pack cont)
      case parsed of
        Left err -> putStr $ P.errorBundlePretty err
        Right tree -> putStr $ unlines $ runNil tree
    _ -> putStrLn "Usage: nil source.nil"

