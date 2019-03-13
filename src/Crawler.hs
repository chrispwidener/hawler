module Crawler (
    crawlWithDefaults,
    startRequesting,
    Data
) where

-----   IMPORTS

import Parse

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Control.Lens

import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import qualified Data.List as L
--import Data.Maybe
import Data.Set (Set, insert, notMember, empty)
import qualified Data.Time.Clock as C

import qualified Network.Wreq as W

import System.Timeout

---- Data Types/Classes

class Data a where
    getDomain  :: a -> String 
    putResults :: a -> b -> a

    --- API Functions

crawlWithDefaults :: Data a => a -> (ByteString -> a) -> IO a
crawlWithDefaults dat parser = do
    --startRequesting [getDomain data] S.empty 0 
    return dat

--crawlWithOpts :: opts -> Context a -> (B.ByteString -> a) -> IO a


    --- Internal Functions

startRequesting :: String -> Chan ByteString -> IO ()
startRequesting domain sendChan = do
    recvChan <- newChan
    startRequesting' domain [domain] empty recvChan sendChan 

startRequesting' :: String -> [String] -> Set String -> Chan (ByteString) -> Chan (ByteString) -> IO ()
startRequesting' domain urlQueue visitedUrls recvChan sendChan = do
    results <- checkAllChanContents recvChan
    case (results, urlQueue) of
        --  Wait for another request to finish if no results or urls in queue
        ([], []) -> do
            response <- waitThenCheckChan recvChan
            case response of
                Nothing       -> return ()
                Just resp -> do
                    writeChan recvChan resp
                    startRequesting' domain urlQueue visitedUrls recvChan sendChan
        -- Prioritize processing of results prior to initiating new http requests to prevent space leaks
        ((r:rs), _) -> do
            let newLinks = L.filter (`notMember` visitedUrls) $ L.foldr1 (<>) $ map (extractAndRepairUrls domain) (r:rs)
                newUrlQueue = L.nub $ urlQueue ++ newLinks
            startRequesting' domain newUrlQueue visitedUrls recvChan sendChan
        -- Send Requests when there are no responses to process
        (_, (u:us)) -> do
            forkIO $ get u recvChan
            threadDelay (500000)
            startRequesting' domain us (insert u visitedUrls) recvChan sendChan

get :: String -> Chan (ByteString) -> IO ()
get url chan = do
    resp <- try (W.get url) :: IO (Either SomeException (W.Response ByteString))
    case resp of
        Right r -> do
            putStrLn ""
            putStrLn $ "Url: " ++ url
            putStrLn $ "Status code: " ++ (show $ r ^. W.responseStatus)
            putStrLn ""
            writeChan chan $ r ^. W.responseBody
        Left e  -> do
            putStrLn ""
            putStrLn $ "Url: " ++ url
            putStrLn $ "Error: " ++ (L.take 50 $ show e) 
            putStrLn ""


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