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
import           Database.Persist.Sql
import qualified Data.Conduit.List as CL
import Yesod.Table (Table)
import qualified Yesod.Table as Table
import Data.Monoid ((<>))
import Debug.Trace
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.), (?.))
import CardReader.PersistDerived (CardNum(..))
import Data.Maybe

--getCardReaderR :: (Yesod master, YesodPersist master, YesodPersistBackend master ~ SqlBackend) => HandlerT CardReader (HandlerT master IO) Html
getCardReaderR reportType = do
  result <- getReport reportType
  let tableWidget = Table.buildBootstrap dayTable rows
      rows = fmap (\(E.Value fn, E.Value ln, E.Value ct, E.Value mn, E.Value dt) -> DayRow fn ln ct mn dt) result

  lift $ defaultLayout
       [whamlet|
                ^{tableWidget}
       |]
  -- webSockets $
  --   sendTextData $ encode report

getReport Day =
   lift $ runDB
        $ E.select
        $ E.from $ \(person, message, event) -> do
              E.where_ ((lhs person E.==. rhs message) E.&&. ((message ^. MessageId) E.==. (event ^. ReaderEventMessage)))
              return $ ( person ^. PersonFirstName
	      	       , person ^. PersonLastName
 	      	       , person ^. PersonCardType
	      	       , message ^. MessageCardNum
		       , event ^. ReaderEventDate
	      	       )

 where
   lhs person = E.just (person ^. PersonNumber)
   rhs message = (message ^. MessageCardNum)

dayTable = mempty
  <> Table.text "First Name" dayRowFName
  <> Table.text "Last Name" dayRowLName
  <> Table.text "Contractor/Employee" (tshow . dayRowCardType)
  <> Table.text "Badge ID" (tshow . fromJust . dayRowCardNum)
  <> Table.text "Timestamp" (tshow . dayRowUtc)

instance (Yesod master, YesodPersist master, YesodPersistBackend master ~ SqlBackend) => YesodSubDispatch CardReader (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesCardReader)

