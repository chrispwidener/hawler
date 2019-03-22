module Crawl where

-----   IMPORTS

import Parse

import Control.Concurrent
import Control.Exception
import Control.Lens

import Data.ByteString.Lazy (ByteString)
--import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.List (nub, sort)
import Data.Set (Set, insert, notMember, empty, size)

import qualified Network.Wreq as W

import System.Timeout

---- Data Types/Classes

data CrawlOpts a = CrawlOpts {
      delay :: Int
    , sChan :: Chan (Id, Domain, [a])
    , rChan :: Chan ByteString
    , limit :: Int
}

type Id          = Integer
type Domain      = String
type Url         = String
type UrlQueue    = [String]
type Parser a    = ByteString -> a
type VisitedUrls = Set String

    --- API Functions

crawlDefaults :: Ord a => Id-> Domain -> Parser [a] -> Chan (Id, Domain, [a]) -> IO ()
crawlDefaults id' domain parser sendChan = do
    recvChan <- newChan
    let opts = CrawlOpts 1000 sendChan recvChan 0
    crawlWithOpts id' domain parser opts

crawlWithOpts :: Ord a => Id -> Domain -> Parser [a] -> CrawlOpts a -> IO ()
crawlWithOpts id' domain parser opts = do
    _ <- forkIO $ startRequesting id' domain parser [domain] empty opts []
    return ()

    --- Internal Functions

startRequesting :: Ord a => Id -> Domain -> Parser [a] -> UrlQueue -> VisitedUrls -> CrawlOpts a -> [a] -> IO ()
startRequesting id' domain parse urlQueue visitedUrls opts results = do
    let recvChan  = rChan  opts
        sendChan  = sChan  opts
        deelay    = delay  opts
        pageLimit = limit  opts

    --- Break if pageLimit is set and we're over the limit
    if pageLimit /= 0 && size visitedUrls > pageLimit 
        then do 
            writeChan sendChan (id', domain, results)
            return ()
        else do 

            resps <- checkAllChanContents recvChan
            case (resps, urlQueue) of
                --  Wait for another request to finish if no results or urls in queue
                ([], []) -> do
                    response <- waitThenCheckChan recvChan
                    case response of
                        Nothing   -> do
                            writeChan sendChan (id', domain, results)
                            return ()
                        Just resp -> do
                            writeChan recvChan resp
                            startRequesting id' domain parse urlQueue visitedUrls opts results
                -- Prioritize processing of results prior to initiating new http requests to prevent space leaks
                ((r:rs), _) -> do
                    let newLinks = filter (`notMember` visitedUrls) $ foldr1 (<>) $ map (extractAndRepairUrls domain) (r:rs)
                        newUrlQueue = nub $ urlQueue ++ newLinks
                        parseResult = parse r
                        newResults  = sort $ nub $ results ++ parseResult
                    startRequesting id' domain parse newUrlQueue visitedUrls opts $! newResults
                -- Send Requests when there are no responses to process
                (_, (u:us)) -> do
                    _ <- forkIO $ get u recvChan
                    threadDelay (deelay * 1000)
                    startRequesting id' domain parse us (insert u visitedUrls) opts results

get :: String -> Chan (ByteString) -> IO ()
get url chan = do
    --let userAgent = B.pack "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.113 Safari/537.36"
        --opts = W.defaults & W.header hUserAgent .~ [userAgent]
    resp <- try (W.get url) :: IO (Either SomeException (W.Response ByteString))
    case resp of
        Right r -> do
            --putStrLn ""
            --putStrLn $ "Url: " ++ url
            --putStrLn $ "Status code: " ++ (show $ r ^. W.responseStatus)
            --putStrLn ""
            writeChan chan $ r ^. W.responseBody
        Left _  -> do
            --putStrLn ""
            --putStrLn $ "Url: " ++ url
            --putStrLn $ "Error: " ++ (take 50 $ show e) 
            putStrLn "e"
            return ()


checkAllChanContents :: Chan a -> IO [a]
checkAllChanContents chan = do
    x <- checkChan chan
    case x of
        Nothing   -> return []
        Just item -> do
            rest <- checkAllChanContents chan
            return $ item : rest

waitThenCheckChan :: Chan a -> IO (Maybe a)
waitThenCheckChan chan = do
    threadDelay (5000000)  
    x <- checkChan chan
    return x

checkChan :: Chan a -> IO (Maybe a)    
checkChan chan = do
    x <- timeout (55000) (readChan chan)
    return x