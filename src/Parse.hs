{-# LANGUAGE QuasiQuotes #-}

module Parse where

import Control.Lens ((^.))

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Char (chr, toLower)
import Data.List --(nub)
import Data.Maybe (fromJust)

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
import Text.RE.TDFA.ByteString.Lazy


---- Provided Parsers
--  Would like to build the following default parsers:
--  1: Email
--  2: Phone number
--  3: Social Media links


emailParser :: ByteString -> [String]
emailParser resp = extractAllEmails tags
    where tags = parseTags resp


    --where mailtos = getMailtoEmails resp


------------------------------
--------- Url Related --------
------------------------------


extractAndRepairUrls :: String -> ByteString -> [String]
extractAndRepairUrls domain httpResp = filter (isOnDomain domain) $ map (sanitizeUrl domain) $ extractLinks $ parseTags httpResp

isOnDomain :: String -> String -> Bool
isOnDomain domain url = all (==True) $ zipWith (==) (dropUrlPrefix domain) (dropUrlPrefix url)

dropUrlPrefix :: String -> String
dropUrlPrefix  ('h':'t':'t':'p':'s':':':'/':'/':'w':'w':'w':'.':rest) = rest
dropUrlPrefix  ('h':'t':'t':'p':':':'/':'/':'w':'w':'w':'.':rest)     = rest
dropUrlPrefix  ('h':'t':'t':'p':'s':':':'/':'/':rest) = rest
dropUrlPrefix  ('h':'t':'t':'p':':':'/':'/':rest)     = rest

sanitizeUrl :: String -> String -> String
sanitizeUrl domain url 
    | length url == 0   = domain
    | head url == '/'   = domain' ++ url
    | otherwise         = addHttpIfNeccesary url
    where domain' = if last domain == '/' then init domain else domain

addHttpIfNeccesary :: String -> String
addHttpIfNeccesary url =
    let url' = map toLower url
    in case take 4 url' of
            "http" -> url'
            _      -> "http://" ++ url'

bsToString :: ByteString -> String
bsToString bs = map (chr . fromEnum) (unpack bs)

--  Verified
domPat = [re|(http://)?(www.)?[a-z0-9\-_]{4,25}\.(io|com|org|net|tv|church)|]

------------------------------
--------- Tag Related --------
------------------------------


---- Types of Tags

isLinkTag :: Tag ByteString -> Bool
isLinkTag tag = tagOpen (== pack "a") (\_ -> True) tag

isTextTag :: Tag ByteString -> Bool
isTextTag tag = tagText (const True) tag


---- Extraction of Information from Multiple Tags

extractLinks :: [Tag ByteString] -> [String]
extractLinks tags = map fromJust $ filter (/= Nothing) maybeLinks
    where linkTags   = filter isLinkTag tags
          maybeLinks = map getLinkFromTag linkTags

extractText :: [Tag ByteString] -> [ByteString]
extractText tags = map fromJust $ map getTextFromTag $ filter isTextTag tags


---- Extraction of Information from Singular Tags

getLinkFromTag :: Tag ByteString -> Maybe String 
getLinkFromTag (TagOpen _ attrs) = 
    case length filtered of
        0 -> Nothing
        _ -> Just $ bsToString $ snd $ head filtered
    where filtered = filter (\(att, val) -> att == pack "href") attrs

getTextFromTag :: Tag ByteString -> Maybe ByteString
getTextFromTag (TagText x) = Just x
getTextFromTag _           = Nothing

extractPatternFromTextTags :: RE -> [Tag ByteString] -> [String]
extractPatternFromTextTags pat tags = map unpack $ foldr1 (<>) regmatches
        where textList = extractText tags
              regmatches = map (\str -> matches $ str *=~ pat) textList

------------------------------
------- Email Related --------
------------------------------

emailRegPat = [re|[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+.[a-z]{2,5}|]

extractAllEmails :: [Tag ByteString] -> [String]
extractAllEmails tags = nub $ extractMailtoEmails tags <> extractTextEmails tags

extractMailtoEmails :: [Tag ByteString] -> [String]
extractMailtoEmails tags = nub $ map (drop 7) mailtos
    where links   = filter (/= Nothing) $ map getLinkFromTag $ filter isLinkTag $ tags
          mailtos = filter (\l -> take 7 l == "mailto:") $ map (map toLower . fromJust) links

extractTextEmails :: [Tag ByteString] -> [String]
extractTextEmails tags = extractPatternFromTextTags emailRegPat tags


------------------------------
------- Phone Related --------
------------------------------

-- Verified
phoneRegPat = [re|\(?[0-9]{3}\)?( |-|.|,)?[0-9]{3}( |-|.|,)[0-9]{4}|]

extractPhoneNumbers :: [Tag ByteString] -> [String]
extractPhoneNumbers tags = extractPatternFromTextTags phoneRegPat tags

