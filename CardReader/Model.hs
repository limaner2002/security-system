{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
module CardReader.Model where

import Prelude ()
import Yesod
import ClassyPrelude.Yesod hiding (FilePath)

import Database.Persist
import Database.Persist.MySQL
import Control.Monad.Trans.Resource
import Database.Persist.TH
import Text.ParserCombinators.ReadP hiding (pfail)
import Text.Read hiding (get)

import CardReader.PersistDerived

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
ReaderEvent
    date UTCTime
    event Event
    message MessageId
    deriving Show
Person
    number CardNum
    lastName Text
    firstName Text
    cardType CardType
    expires UTCTime Maybe
    info1 Text Maybe
    info2 Text Maybe
    activated UTCTime
    Primary number
    deriving Show
Message
    door Door Maybe
    cardNum CardNum Maybe
    other Text Maybe
    deriving Show
DayRow
    fName Text
    lName Text
    cardType Text
    cardNum Int
    utc UTCTime
    deriving Read Show
|]

instance Read Message where
    readsPrec d v =
        case result of
          [(door, rem)] ->
              case readCard rem of
                [(cardNum, rem1)] -> return (Message (Just door) (Just cardNum) Nothing, rem1)
                _ -> return (Message Nothing Nothing $ Just $ pack v, "")
          _ -> return (Message Nothing Nothing $ Just $ pack v, "")
        where
        result = readsPrec d v :: [(Door, String)]
        readCard rem = readsPrec d rem :: [(CardNum, String)]

data ReportType = Totals
                | Day
                deriving (Show, Read, Eq)

instance PathPiece ReportType where
    fromPathPiece = readMay
    toPathPiece = tshow

-- Yesod stuff

data CardReader = CardReader

-- We have a familiar analogue from mkYesod, with just one extra parameter.
-- We'll discuss that later.
mkYesodSubData "CardReader" [parseRoutes|
/#ReportType CardReaderR GET
|]
