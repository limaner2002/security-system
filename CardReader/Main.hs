{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies               #-}

import Prelude ()
import ClassyPrelude
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.Combinators as C
import Data.Conduit
import Data.CSV.Conduit
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Data.Time
import Database.Persist
import Database.Persist.MySQL
import Control.Monad.Logger (runNoLoggingT, NoLoggingT)

import CardReader.Model

read txt = case readMay txt of
             Nothing -> error ("Could not parse " <> unpack txt)
             Just val -> val

readDate :: Text -> Text -> UTCTime
readDate day time =
    case parsedTime of
      Nothing -> error ("Could not parse " <> date)
      Just val -> val
  where
    date = unpack (day <> " " <> time)
    parsedTime = parseTimeM True defaultTimeLocale "%-m/%e/%Y %k:%M:%S" date

parseReaderEvents :: Row Text -> ReaderEvent
parseReaderEvents (date:time:event:message:_) =
    ReaderEvent (readDate date time)
                    (read event)
                    (read message)

consumer :: ConduitM [Text] o (ReaderT SqlBackend (NoLoggingT (ResourceT IO))) ()
consumer =
    C.mapM_ (\(date:time:event:_:_:message:_) -> do
               let msg = read message :: Message
               msgId <- insert msg
               let readerEvent = ReaderEvent
                                  (readDate date time)
                                  (read event)
                                  msgId
               insert_ readerEvent
            )

-- consumer :: (MonadIO m, MonadReader m) => Consumer (Row Text) m ()
--consumer =
--    C.mapM_ insert_

main :: IO ()
main = do
  (path:args) <- getArgs
  let conn = defaultConnectInfo {connectUser = "josh", connectDatabase = "test"}
  runResourceT . runNoLoggingT . (withMySQLConn conn) . runSqlConn $ do
         runMigration migrateAll
         CB.sourceFile (unpack path) $= intoCSV (defCSVSettings {csvSep=','}) $$ consumer
