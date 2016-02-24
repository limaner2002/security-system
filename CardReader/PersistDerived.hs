{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CardReader.PersistDerived where

import Database.Persist hiding (get)
import Database.Persist.Sql hiding (get)
import Database.Persist.TH
import Prelude ()
import ClassyPrelude.Yesod hiding (get)
import Data.Aeson.TH
import Web.HttpApiData

import Text.ParserCombinators.ReadP hiding (pfail)
import Text.Read hiding (get)

data Event = Event
           | Alarm
    deriving (Show, Read, Eq)



data CardType = Employee
              | Visitor
              | Supervisor
	      | Cleaning
	      | VIP
    deriving (Show, Read)

data Door = Front
          | Back
          | IT
      deriving Show

instance Read Door where
    readsPrec _ v =
        case (readP_to_S $ manyTill get $ string " Card #") v of
          [("Front", rem)] -> return (Front, rem)
          [("IT", rem)] -> return (IT, rem)
          [("Back", rem)] -> return (Back, rem)
          _ -> return (Front, v)

data CardNum = CardNum Int
     deriving (Eq, Ord)

instance Show CardNum where
    show (CardNum x) = show x

instance Read CardNum where
    readsPrec d v =
      case readsPrec d lexeme of
        [(num, rem)] -> return (CardNum num, rem)
        _ -> []
        where
          [(lexeme, _)] = lex v

instance PathPiece CardNum where
    toPathPiece = toPathPiece 
    fromPathPiece txt = fmap CardNum $ fromPathPiece txt

instance ToHttpApiData CardNum where
    toUrlPiece (CardNum x) = toUrlPiece x

instance FromHttpApiData CardNum where
    parseUrlPiece txt = fmap CardNum $ parseUrlPiece txt

instance PersistFieldSql CardNum where
    sqlType _ = SqlInt64

instance PersistField CardNum where
    toPersistValue (CardNum n) = toPersistValue n
    fromPersistValue pv = fmap CardNum $ fromPersistValue pv

derivePersistField "CardType"
derivePersistField "Event"
derivePersistField "Door"
-- derivePersistField "CardNum"
$(deriveJSON defaultOptions ''CardNum)