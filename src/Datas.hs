module Datas (
    Operator(..),
    Type(..),
    Abstract(..)
) where

data Operator =   Sum
                | Rest
                | Mult
                | Div
                | Pow
                | Err String
    deriving (Show, Eq)

data Type = TypeOp Operator
            | TypeNum Double
            | TypeLeftPar
            | TypeRightPar
            | TypeErr String
            | TypeEnd
    deriving (Show, Eq)

data Abstract = SumOp Operator Abstract Abstract
                | RestOp Operator Abstract Abstract
                | MultOp Operator Abstract Abstract
                | DivOp Operator Abstract Abstract
                | PowOp Operator Abstract Abstract
                | UnOp Operator Abstract
                | Num Double
                | Error String
    deriving Show