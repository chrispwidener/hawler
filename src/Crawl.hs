module Crawl 
(

  -- Functions to Use Crawler
    crawlDefaults
  , crawlWithOpts

  -- Crawler Options
  , Option 
    (
         Delay
       , Depth
       , Limit
       , Prioritize
    )

) where


import Parse

import Control.Concurrent
import Control.Exception
import Control.Lens

import Data.ByteString.Lazy (ByteString)
import Data.List (nub, partition, isSubsequenceOf)
import Data.Set (Set, insert, notMember, empty, size)

import qualified Network.Wreq as W

import System.Timeout


--------------------
---- Types
--------------------


type Id          = Integer
type Domain      = String
type Url         = String
type UrlQueue    = [Url]
type Parser a    = ByteString -> a
type VisitedUrls = Set String


data Option = Delay Int | Limit Int | Depth Int | Prioritize [String] deriving (Eq)

--------------------
---- API Functions
--------------------

{-

Function: crawlDefaults

    Parameters:

        Id :: Int         
                In case your domain isn't the unique identifier, you can 
                include an Id to reference when the crawler sends the results.
                Just set it to 0 if you don't care.

        Domain :: String     
                The site to crawl, it needs to include 'http://' and be the 'root' 
                otherwise it will not work.
                    IE: 
                        Good:
                            http://google.com/
                        Bad:
                            google.com
                            http://google.com/news

        Parser :: Eq a => ByteString -> a
                The crawler uses Network.Wreq to get the response and 
                ^. responseBody to pass the body of the response to your parser.
                The crawler returns a list of all the 'a's found while parsing.
                The Eq requirement is so the crawler can de-dup the results as
                it's crawling and extracting results

        Chan :: Chan (Id, Domain [a])
                The crawler will use this channel to return a tuple containing the Id, 
                the Domain and the list of results the parser found.  You simply need 
                to bind the result of 'Control.Concurrent.newChan' to a variable and 
                pass it to this function.  

                The reason this function accepts, rather than returns a channel is so
                that you can pass the channel to multiple domain-crawlers running
                concurrently.
                
    Defaults:

        -1000 Millisecond delay between requests to the same domain
        -No limit to the number of urls the crawler can visit on the domain
        -No Depth limit
        -No prioritized url substrings

    Usage Example:

        import Control.Concurrent (newChan, readChan)
        import Crawl (crawlDefaults)
        import Parse (emailParser)

        main :: IO ()
        main = do
            let domain = "http://example-website.com"
            chan <- newChan
            crawlDefaults 0 domain emailParser chan
            (_, _, emails) <- readChan chan
            mapM_ print emails

-}

crawlDefaults :: Eq a => Id-> Domain -> Parser [a] -> Chan (Id, Domain, [a]) -> IO ()
crawlDefaults id' domain parser sendChan = crawlWithOpts id' domain parser sendChan []

{-

Function: crawlWithOpts

    Parameters:
            
        All of the parameters from crawlDefaults plus:

        [Option]:
                Option is a data type with constructors:
                    Delay Int: 
                        Millisecond delay between requests to the same domain.
                    Limit Int: 
                        Max number of sub-pages the crawler will visit on one domain.
                    Depth Int: 
                        Max 'depth' the crawler will dive into links.
                    Prioritize [String]: 
                        A list of substrings that, when contained in a url, will cause 
                        the url to be pushed to the front of the url queue.

    Defaults:

            The same defaults as crawlDefaults, crawlDefaults just calls this
            function with the [Option] parameter set to [].

    Usage Example:

        import Control.Concurrent (newChan, readChan)
        import Crawl (crawlWithOpts, Options (..))
        import Parse (emailParser)

        main :: IO ()
        main = do
            let domain = "http://example-website.com"
            chan <- newChan
            crawlWithOpts 0 domain emailParser chan [Limit 100, Depth 2, Delay 500]
            (_, _, emails) <- readChan chan
            mapM_ print emails

-}

crawlWithOpts :: Eq a => Id -> Domain -> Parser [a] -> Chan (Id, Domain, [a]) -> [Option] -> IO ()
crawlWithOpts id' domain parser sendChan opts = do
    defs   <- defaults domain sendChan
    let moddedOpts = modifyOpts defs opts
    _ <- forkIO $ startRequesting id' moddedOpts parser []
    return ()


------------------------------
---- Internal Functions
------------------------------

startRequesting :: Eq a => Id -> CrawlConfig a -> Parser [a] -> [a] -> IO ()
startRequesting id' opts parse results = do
    let domain      = dom   opts
        recvChan    = rChan opts
        sendChan    = sChan opts
        delay       = del   opts
        pageLimit   = limit opts
        priorities  = prio  opts
        urlQueue    = urlQ  opts
        visitedUrls = vUrls opts

    --- Reaturn current results if pageLimit is set and we're over the limit
    if pageLimit /= 0 && size visitedUrls > pageLimit 
        then do 
            writeChan sendChan (id', domain, results)
            return ()
        else do 
            resps <- checkAllChanContents recvChan

            case (resps, urlQueue) of
                --  If there are no results to process or urls in queue then
                --  wait 10 seconds for another request to finish 
                ([], []) -> do
                    response <- waitThenCheckChan recvChan
                    case response of
                        Nothing   -> do
                            writeChan sendChan (id', domain, results)
                            return ()
                        Just resp -> do
                            writeChan recvChan resp
                            startRequesting id' opts parse results

                -- Prioritize processing of results prior to initiating 
                -- new http requests.  This should help prevent space leaks
                ((r:rs), _) -> do
                    let newLinks = filter (`notMember` visitedUrls) $ foldr1 (<>) $ map (extractAndRepairUrls domain) (r:rs)
                        newUrlQueue = nub $ urlQueue ++ newLinks
                        parseResult = parse r
                        newResults  = nub $ results ++ parseResult
                        newOpts     = opts { urlQ = prioritizeUrlQueue priorities newUrlQueue }
                    startRequesting id' newOpts parse $! newResults

                -- Send Requests when there are no responses to process
                (_, (u:us)) -> do
                    _ <- forkIO $ get u recvChan
                    threadDelay (delay * 1000)
                    let newOpts = opts { urlQ = us, vUrls = insert u visitedUrls }
                    startRequesting id' newOpts parse results


get :: String -> Chan (ByteString) -> IO ()
get url chan = go 1 url chan
    where 
        go 4 _ _            = return ()
        go numAtts url chan = do
            resp <- try (W.get url) :: IO (Either SomeException (W.Response ByteString))
            case resp of
                Right r -> do
                    writeChan chan $ r ^. W.responseBody
                Left _  -> do
                    go (numAtts + 1) url chan


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


data CrawlConfig a = CrawlConfig {
      dom   :: Domain
    , sChan :: Chan (Id, Domain, [a]) -- This is the chan the crawler will send results to.
    , rChan :: Chan ByteString        -- This is the internal chan the get requests will use to send responses.
    , del   :: Int                    -- Delay between requests to same domain (Milliseconds).
    , limit :: Int                    -- Page visit limit for one domain, if it's 0 there is no limit.
    , depth :: Int                    -- The link depth limit, main page is depth 0, links from there are depth 1, etc.
    , prio  :: [String]               -- A list of substrings that, if present in a url, move it to the top of the queue.
    , urlQ  :: UrlQueue               -- The queue of urls we have yet to visit.
    , vUrls :: VisitedUrls            -- The urls already visited on a site.
} 

instance Show (CrawlConfig a) where
    show (CrawlConfig dom _ _ del lim dep pri _ _) = 
        "Options:\nDelay: " ++ show del ++ 
        "\nLimit: " ++         show lim ++ 
        "\nDepth: " ++         show dep ++ 
        "\nPriorities: " ++    show pri

-- Pass in a domain and the return chan to get defaults
defaults :: Domain -> Chan (Id, Domain, [a]) -> IO (CrawlConfig a)
defaults domain chan = do
    receiveChan <- newChan
    return $ CrawlConfig {
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

modifyOpts :: CrawlConfig a -> [Option] -> CrawlConfig a
modifyOpts def []     = def
modifyOpts def (o:os) =
    let o' = sanitizeOpt o
    in case o' of
        Delay x       -> modifyOpts (def { del   = x }) os
        Limit x       -> modifyOpts (def { limit = x }) os
        Depth x       -> modifyOpts (def { depth = x }) os
        Prioritize xs -> modifyOpts (def { prio  = xs }) os

sanitizeOpt :: Option -> Option
sanitizeOpt (Delay x) = Delay $ if x < 300 then 300 else x
sanitizeOpt (Limit x) = Limit $ if x < 0   then 0   else x
sanitizeOpt (Depth x) = Depth $ if x < 0   then 0   else x
sanitizeOpt opt = opt


prioritizeUrlQueue :: [String] -> UrlQueue -> UrlQueue
prioritizeUrlQueue prios queue = prioritized ++ rest
    where (prioritized, rest) = partition (\url -> any (\prio -> isSubsequenceOf prio url) prios) queue