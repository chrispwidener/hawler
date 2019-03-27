# hawler

## Description:

Hawler is a library for crawling one or more websites to parse and extract information.  The crawler settings are configurable to a certain degree, and some default parsers are included 

## API

### Hawler Crawl exports the following:

```
crawlDefaults :: Eq a => Integer -> String -> (ByteString -> a) -> Chan (Integer, String, [a]) -> IO ()

crawlWithOpts :: Eq a => Integer -> String -> (ByteString -> a) -> Chan (Integer, String [a]) -> [Option] -> IO ()

-- Option is a datatype for creating Crawler Options, it is defined like so:
data Option =
      Delay Int
    | Limit Int
    | Depth Int
    | Prioritize [String]
```

- The Integer is an ID that the crawler will include in the results of the crawl, this is useful if the site itself is not a unique identifier for saving the results.

- The String is the Site itself.

- The ByteString -> a function is the parser that takes the http response and converts it to your results.

### Hawler Parse exports the following:

`emailParser :: ByteString -> [String]`

This is a default parser that you can plug into the crawler to retrieve email addresses

## Usage Examples

Crawling a single domain, retrieved from the command line, with defaults:

```
module Main where

import Crawl
import Parse (emailParser)

import Control.Concurrent (Chan, newChan, readChan)

import System.Environment

main :: IO ()
main = do
    [site] <- getArgs
    chan <- newChan
    crawlDefaults 0 site emailParser chan
    (id', site, emails) <- readChan chan
    mapM_ putStrLn emails

```

Crawling multiple domains in parallel, with custom crawl options.

```
module Main where

import Crawl
import Parse (emailParser)

import Control.Concurrent (Chan, newChan, readChan)

import System.Environment

main :: IO ()
main = do
    chan <- newChan
    let opts = [Limit 50, Delay 500, Prioritize ["contact", "staff"]]
    mapM_ (\site -> crawlWithOpts 0 site emailParser chan opts) sites
    waitReadPrint (length urls) chan

sites = [
    "http://examplesite1.com",
    "http://examplesite2.com",
    "http://examplesite3.com",
    "http://examplesite4.com",
    "http://examplesite5.com",
    "http://examplesite6.com",
    "http://examplesite7.com",
    "http://examplesite8.com",
    "http://examplesite9.com",
    "http://examplesite10.com",
]

waitReadPrint :: Int -> Chan (Integer, String, [String]) -> IO ()
waitReadPrint 0 _    = return ()
waitReadPrint x chan = do
    (_, url, emails) <- readChan chan
    putStrLn $ "--\n\nFound the following emails on " ++ url
    mapM_ print emails
    putStr "\n\n"
    waitReadPrint (x-1) chan

```