module Main where

import Lib
import System.Environment
import ErrorHandling
import MainProcess

main :: IO ()
main = do
        args <- getArgs
        name <- getProgName 
        case length args of
            1 -> mainProcess (head args)
            _ -> displayUsage name
