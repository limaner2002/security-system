{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module CardReader.PersistDerived where

import Database.Persist.TH
import Prelude ()
import ClassyPrelude.Yesod hiding (get)

import Text.ParserCombinators.ReadP hiding (pfail)
import Text.Read hiding (get)

data Event = Event
           | Alarm
    deriving (Show, Read, Eq)



data CardType = Employee
              | Visitor
              | Supervisor
    deriving (Show, Read)

data Door = Front
          | Back
          | IT
      deriving Show

instance Read Door where
    readsPrec _ v =
        case (readP_to_S $ manyTill get $ string " Card #") v of
          [("Front Entry Reader", rem)] -> return (Front, rem)
          [("IT Room Reader", rem)] -> return (IT, rem)
          [("Reader #3", rem)] -> return (Back, rem)
          _ -> return (Front, v)

data CardNum = CardNum Int

instance Show CardNum where
    show (CardNum x) = show x

instance Read CardNum where
    readsPrec d v =
      case readsPrec d lexeme of
        [(num, rem)] -> return (CardNum num, rem)
        _ -> []
        where
          [(lexeme, _)] = lex v

derivePersistField "CardType"
derivePersistField "Event"
derivePersistField "Door"
derivePersistField "CardNum"
