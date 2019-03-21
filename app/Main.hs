{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

import Crawl
import Parse 

import Control.Concurrent.Chan

import System.IO

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    receiveChan  <- newChan
    internalChan <- newChan
    crawlWithOpts "http://www.gracecitychurch.com" emailParser (CrawlOpts 1000 receiveChan internalChan 50)
    waitAndPrint receiveChan

waitAndPrint :: Chan (String, [String]) -> IO ()
waitAndPrint chan = do
    (dom, results) <- readChan chan
    putStrLn $ "Crawled " ++ dom ++ " and found the following emails:"
    mapM_ print results
