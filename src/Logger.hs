module Logger where

import Config (Configuration (logOutput, priorityLevel), Priority (..), buildTime, logFile)
import Control.Monad.Reader (asks, liftIO)
import Environment (Environment (configuration), Flow)

-- Function writes log information down.
writingLine :: Priority -> String -> Flow ()
writingLine level str = do
  conf <- asks configuration
  if level >= priorityLevel conf
    then liftIO $ do
      time <- buildTime
      let string = time <> " UTC   " <> showLevel level <> " - " <> str
          out = logOutput conf
      case out of
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
writingLineDebug s = writingLine DEBUG $ show s
