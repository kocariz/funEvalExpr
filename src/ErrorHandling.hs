module ErrorHandling (
    displayUsage
) where

import System.Exit

displayUsage :: String -> IO()
displayUsage x =
        putStr ("USAGE:\t./" ++ x ++ " math-expression\n\t\tInput a ") >>
        putStr "valid mathematical expression following the example" >>
        putStr ("\n\t\tExemple:\t./" ++ x ++ " \"33 + 21 * ") >>
        putStr "(19 - 51)\"\n" >>
        exitWith (ExitFailure 84)