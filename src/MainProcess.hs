module MainProcess(
    mainProcess
) where

import Parser
import AbstractSyntaxTree
import Calculate
import Text.Printf
import System.Exit (exitWith, ExitCode (ExitFailure))

mainProcess :: String -> IO()
mainProcess args = let types = getTypes args in
    let abstractTree = setAbstractTree types in
    let calculs = calculate abstractTree in
    case calculs of
        Right one-> printf "%.2f\n" one
        Left msg -> print msg >> exitWith (ExitFailure 84)