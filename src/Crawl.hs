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

type Id          = Integer
type Domain      = String
type Url         = String
type UrlQueue    = [String]
type Parser a    = ByteString -> a
type VisitedUrls = Set String

    --- API Functions

crawlDefaults :: Ord a => Id-> Domain -> Parser [a] -> Chan (Id, Domain, [a]) -> IO ()
crawlDefaults id' domain parser sendChan = do
    crawlWithOpts id' domain parser sendChan []

crawlWithOpts :: Ord a => Id -> Domain -> Parser [a] -> Chan (Id, Domain, [a]) -> [Option] -> IO ()
crawlWithOpts id' domain parser sendChan opts = do
    defs   <- defaults domain sendChan
    let moddedOpts = modifyDefaults defs opts
    _ <- forkIO $ startRequesting id' moddedOpts parser []
    return ()

    --- Internal Functions

startRequesting :: Eq a => Id -> CrawlOpts a -> Parser [a] -> [a] -> IO ()
startRequesting id' opts parse results = do
    let domain      = dom   opts
        recvChan    = rChan opts
        sendChan    = sChan opts
        delay       = del   opts
        pageLimit   = limit opts
        urlQueue    = urlQ  opts
        visitedUrls = vUrls opts

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
                            startRequesting id' opts parse results
                -- Prioritize processing of results prior to initiating new http requests to prevent space leaks
                ((r:rs), _) -> do
                    let newLinks = filter (`notMember` visitedUrls) $ foldr1 (<>) $ map (extractAndRepairUrls domain) (r:rs)
                        newUrlQueue = nub $ urlQueue ++ newLinks
                        parseResult = parse r
                        newResults  = nub $ results ++ parseResult
                        newOpts     = opts { urlQ = newUrlQueue }
                    startRequesting id' newOpts parse $! newResults
                -- Send Requests when there are no responses to process
                (_, (u:us)) -> do
                    _ <- forkIO $ get u recvChan
                    threadDelay (delay * 1000)
                    let newOpts = opts { urlQ = us, vUrls = insert u visitedUrls }
                    startRequesting id' newOpts parse results

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

------ Options

data Option = Delay Int | Limit Int | Depth Int | Prioritize [String] deriving (Eq)

data CrawlOpts a = CrawlOpts {
      dom   :: Domain
    , sChan :: Chan (Id, Domain, [a]) -- This is the chan the crawler will send results to
    , rChan :: Chan ByteString        -- This is the internal chan the get requests will use to send responses
    , del   :: Int                    -- Delay between requests to same domain (Milliseconds)
    , limit :: Int                    -- Page visit limit for one domain, if it's 0 there is no limit
    , depth :: Int                    -- 
    , prio  :: [String]
    , urlQ  :: UrlQueue
    , vUrls :: VisitedUrls
} 

instance Show (CrawlOpts a) where
    show (CrawlOpts dom _ _ del lim dep pri _ _) = 
        "Options:\nDelay: " ++ show del ++ 
        "\nLimit: " ++         show lim ++ 
        "\nDepth: " ++         show dep ++ 
        "\nPriorities: " ++    show pri

-- Pass in a domain and the return chan to get defaults
defaults :: Domain -> Chan (Id, Domain, [a]) -> IO (CrawlOpts a)
defaults domain chan = do
    receiveChan <- newChan
    return $ CrawlOpts {
                  dom    = domain
                , sChan  = chan 
                , rChan  = receiveChan 
                , del    = 1000
                , limit  = 0 
                , depth  = 0 
                , urlQ   = [domain]
                , vUrls  = empty
                , prio   = []
             }

modifyDefaults :: CrawlOpts a -> [Option] -> CrawlOpts a
modifyDefaults def opts = go def opts
    where 
        go :: CrawlOpts a -> [Option] -> CrawlOpts a
        go opt []     = opt
        go opt (o:os) =
            case o of
                Delay x       -> go (opt { del   = x }) os
                Limit x       -> go (opt { limit = x }) os
                Depth x       -> go (opt { depth = x }) os
                Prioritize xs -> go (opt { prio  = xs }) os
