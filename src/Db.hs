{-# LANGUAGE OverloadedStrings #-}

module Db where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

dbPath = "C:\\Dev\\00 - Data\\churchesdup.db"

type Churches = [Church]

data Church = 
    Church {
        id_            :: Integer,
        name           :: Maybe String,
        denomination   :: Maybe String,
        address        :: Maybe String,
        city           :: Maybe String,
        state          :: Maybe String,
        cfUrl          :: Maybe String,
        realUrl        :: Maybe String,
        jmcUrl         :: Maybe String,
        facebookUrl    :: Maybe String,
        twitterUrl     :: Maybe String,
        givingTool     :: Maybe String,
        cmsTool        :: Maybe String,
        websites       :: Maybe String,
        emailAddresses :: Maybe String,
        phoneNumbers   :: Maybe String
    }

instance Show Church where
    show church = 
        go (name church) ++ ": " ++ go (denomination church)
        where 
            go (Just str) = str
            go Nothing    = ""

instance FromRow Church where
    fromRow = Church <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

--printSomeChurches :: IO ()
getChurches = do
    conn <- open dbPath
    r <- query_ conn "SELECT * FROM churches limit 100" :: IO Churches
    close conn
    return r