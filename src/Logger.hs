module Logger where

import Config (Priority (..), buildTime, logFile)
import Control.Monad.Reader (ask, liftIO)
import Environment (Environment (logOutputObject, loggingLevel), Flow)

-- Function writes log information down.
writingLine :: Priority -> String -> Flow ()
writingLine level information = do
  env <- ask
  if level >= loggingLevel env
    then liftIO $ do
      time <- buildTime
      let string = time <> " UTC   " <> showLevel level <> " - " <> information
          outputDestinationType = logOutputObject env
      case outputDestinationType of
        "file" -> appendFile logFile $ string <> "\n"
        _ -> putStrLn string
    else pure ()
  where
    showLevel priority = case priority of
      DEBUG -> "DEBUG  "
      INFO -> "INFO   "
      WARNING -> "WARNING"
      ERROR -> "ERROR  "

writingLineDebug :: (Show a) => a -> Flow ()
writingLineDebug = writingLine DEBUG . show
