module Parser (
    setOperator,
    getTypes,
    setAbstractTree
) where

import ErrorHandling (displayUsage)
import Data.Char
import Datas
import AbstractSyntaxTree
import Calculate

setOperator :: Char -> Operator
setOperator '+' = Sum
setOperator '-' = Rest 
setOperator '*' = Mult 
setOperator '/' = Div 
setOperator '^' = Pow
setOperator c = Err $ "Invalid character: " ++ [c]

isDecimal :: Char -> Bool
isDecimal c = c `elem` "0123456789."

toNumber :: String -> Char -> [Type]
toNumber xs x = let (digs, xs') = span isDecimal xs in
    TypeNum(read(x : digs) :: Double) : getTypes xs'

getTypes :: String -> [Type]
getTypes [] = []
getTypes ('(' : next) = TypeLeftPar : getTypes next
getTypes (')' : next) = TypeRightPar : getTypes next
getTypes (current : next)
    | current `elem` "+-*/^" = TypeOp (setOperator current) : getTypes next
    | isNumber current = toNumber next current
    | isSpace current = getTypes next
    | otherwise = [TypeErr ("Invalid type: " ++ [current])]