{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
module CardReader.CardReader
    ( module CardReader.Model
    , module CardReader.CardReader
    ) where

import           CardReader.Model
import           Yesod
import           ClassyPrelude.Yesod hiding ((<>))
import           Yesod.WebSockets
import           Data.Aeson
import           Database.Persist.Sql
import qualified Data.Conduit.List as CL
import Yesod.Table (Table)
import qualified Yesod.Table as Table
import Data.Monoid ((<>))
import Debug.Trace
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.), (?.))
import CardReader.PersistDerived (CardNum(..))

--getCardReaderR :: (Yesod master, YesodPersist master, YesodPersistBackend master ~ SqlBackend) => HandlerT CardReader (HandlerT master IO) Html
getCardReaderR reportType = do
  reportRows <- getReport reportType
  -- let tableWidget = Table.buildBootstrap dayTable rows
  --     rows = map entityVal reportRows
  lift $ defaultLayout
       [whamlet|
                 <ul>
                     $forall (E.Value personFirstName, E.Value personLastName, E.Value personCardType, E.Value messageCardNum) <- reportRows
       |]
  -- webSockets $
  --   sendTextData $ encode report

getReport Day =
   lift $ runDB
        $ E.select
        $ E.from $ \(person, message) -> do
              E.where_ (lhs person E.==. rhs message)
              return
                ( person ^. PersonLastName
                , person ^. PersonFirstName
                , person ^. PersonCardType
                , person ^. PersonNumber
                )
 where
   lhs person = E.just (person ^. PersonNumber)
   rhs message = (message ^. MessageCardNum)
-- getReport reportType = lift $ runDB $ rawSql (trace (s reportType) (s reportType)) []
--     where
      -- s Day =
      --     -- "SELECT ?? FROM person p INNER JOIN message m ON m.card_num = p.number INNER JOIN reader_event e ON e.message = m.id ORDER BY p.last_name,p.first_name;"
      --     "SELECT p.first_name,p.last_name,p.card_type,p.number,e.date FROM person p INNER JOIN message m ON m.card_num = p.number INNER JOIN reader_event e ON e.message = m.id ORDER BY p.last_name,p.first_name;"

-- dayTable = mempty
--   <> Table.text "First Name" dayRowFName
--   <> Table.text "Last Name" dayRowLName
--   <> Table.text "Contractor/Employee" dayRowCardType
--   <> Table.int "Badge ID" dayRowCardNum
--   <> Table.text "Timestamp" (tshow . dayRowUtc)

instance (Yesod master, YesodPersist master, YesodPersistBackend master ~ SqlBackend) => YesodSubDispatch CardReader (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesCardReader)

