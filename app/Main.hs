module Main where

import Crawler
import Control.Concurrent.Chan

import System.Environment
import System.IO

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    [url] <- getArgs
    c <- newChan
    startRequesting url c