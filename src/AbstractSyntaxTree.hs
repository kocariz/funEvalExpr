module AbstractSyntaxTree (
    setAbstractTree
) where

import Datas

getHeadType :: [Type] -> Type
getHeadType [] = TypeEnd
getHeadType (types:_) = types

getTailType :: [Type] -> [Type]
getTailType [] = []
getTailType (_:types) = types

getNums :: Double -> [Type] -> (Abstract, [Type])
getNums num types = case getHeadType (getTailType types) of
        (TypeNum _) ->
            (Error "Missing operand next to number", getTailType types)
        TypeErr str ->
            (Error $ "Missing error unknow token " ++ str, getTailType types)
        _ -> (Num num, getTailType types)

getParenthesis :: [Type] -> (Abstract, [Type])
getParenthesis types = let (_absTree, types_) = firstStep (getTailType types)
    in
        if getHeadType types_ /= TypeRightPar then
            (Error "Missing right parenthesis", getTailType types)
        else (_absTree, getTailType types_)

setAbstractTree :: [Type] -> Abstract
setAbstractTree types = let (absTree, types_) = firstStep types
              in absTree

firstStep :: [Type] -> (Abstract, [Type])
firstStep types = let (absTree, types_) = secondStep types in
    case getHeadType types_ of
        (TypeOp Sum) ->
            let (absTree_, types__) = firstStep (getTailType types_)
            in (SumOp Sum absTree absTree_, types__)
        (TypeOp Rest) ->
            let (absTree_, types__) = firstStep (getTailType types_)
            in (RestOp Rest absTree absTree_, types__)
        _ -> (absTree, types_)

secondStep :: [Type] -> (Abstract, [Type])
secondStep types = let (absTree, types_) = thirdStep types in
    case getHeadType types_ of
        (TypeOp Mult) ->
            let (absTree_, types__) = secondStep (getTailType types_)
            in (MultOp Mult absTree absTree_, types__)
        (TypeOp Div) ->
            let (absTree_, types__) = secondStep (getTailType types_)
            in (DivOp Div absTree absTree_, types__)
        (TypeOp Pow) ->
            let (absTree_, types__) = secondStep (getTailType types_)
            in (PowOp Pow absTree absTree_, types__)
        _ -> (absTree, types_)

thirdStep :: [Type] -> (Abstract, [Type])
thirdStep types = case getHeadType types of
    (TypeNum x) -> getNums x types
    (TypeOp op) | op `elem` [Sum, Rest] ->
        let (absTree, types_) = thirdStep (getTailType types)
        in (UnOp op absTree, types_)
    TypeLeftPar -> getParenthesis types
    _ ->
        (Error ("buildTree error on token: " ++ show types), getTailType types)