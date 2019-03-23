{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

import Crawl
import Parse 

import Control.Concurrent.Chan
import Data.List (unlines)

import System.IO

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    --chan  <- newChan
    --mapM_ (startNewCrawl chan 0) urls
    --waitAndPrint 10 chan

urls :: [String]
urls = [
      "http://www.gracecitychurch.com" 
    , "http://www.svachurch.org"
    , "http://www.churchontheridge.org"
    , "http://www.blueskychurch.com/"
    , "http://doxa-church.com"
    , "http://www.firstpres.org/"
    , "http://www.downtowncornerstone.org/"
    , "http://www.trinityseattle.org/"
    , "http://www.firstchurchseattle.org/"
    , "http://www.blessed-sacrament.org/"
    ]
{-
waitAndPrint :: Int -> Chan (Id, String, [String]) -> IO ()
waitAndPrint 0 _    = return ()
waitAndPrint n chan = do
    (_, dom, results) <- readChan chan
    putStr $ "\nSearched: " ++ dom ++ "and found:\n" ++ unlines results ++ "\n"
    waitAndPrint (n-1) chan

startNewCrawl :: Chan (Id, Domain, [String]) -> Id -> Domain -> IO ()
startNewCrawl chan id' domain = do
    internalChan <- newChan
    crawlWithOpts id' domain emailParser (CrawlOpts 1000 chan internalChan 50)
-}