module Parse where

import Control.Lens

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Char (chr, toLower)
import Data.List (nub)
import Data.Maybe (fromJust)

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match


---- Provided Parsers
--emailParser :: ByteString -> [String]
--emailParser resp =
    --where mailtos = getMailtoEmails resp
          
getMailtoEmails :: [Tag ByteString] -> [String]
getMailtoEmails tags = nub $ map (drop 7) mailtos
    where links   = filter (/= Nothing) $ map getLinkFromTag $ filter isLinkTag $ tags
          mailtos = filter (\l -> take 7 l == "mailto:") $ map (map toLower . fromJust) links


extractAndRepairUrls :: String -> ByteString -> [String]
extractAndRepairUrls domain httpResp = filter (isOnDomain domain) $ map (sanitizeUrl domain) $ extractLinks httpResp

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

extractLinks :: ByteString -> [String]
extractLinks resp = map fromJust $ filter (/= Nothing) maybeLinks
    where tagsoup    = parseTags resp
          linkTags   = filter isLinkTag tagsoup
          maybeLinks = map getLinkFromTag linkTags

isLinkTag :: Tag ByteString -> Bool
isLinkTag tag = tagOpen (== pack "a") (\_ -> True) tag

getLinkFromTag :: Tag ByteString -> Maybe String 
getLinkFromTag (TagOpen _ attrs) = 
    case length filtered of
        0 -> Nothing
        _ -> Just $ bsToString $ snd $ head filtered
    where filtered = filter (\(att, val) -> att == pack "href") attrs

bsToString :: ByteString -> String
bsToString bs = map (chr . fromEnum) (unpack bs)