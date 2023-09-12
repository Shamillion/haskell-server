module Logger where

import Config (Configuration (logOutput, priorityLevel), Priority (..), logFile, time)
import Control.Monad.Reader (ReaderT, asks, liftIO)
import Environment (Environment (configuration))

-- Function writes log information down.
writingLine :: Priority -> String -> ReaderT Environment IO ()
writingLine level str = do
  conf <- asks configuration
  if level >= priorityLevel conf
    then do
      t <- liftIO time
      let string = t <> " UTC   " <> showLevel level <> " - " <> str
          out = logOutput conf
      liftIO $
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

writingLineDebug :: (Show a) => a -> ReaderT Environment IO ()
writingLineDebug s = writingLine DEBUG $ show s
