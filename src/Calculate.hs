module Calculate (
    calculate
) where

import Datas

powCheck :: Either String Double -> Either String Double -> Either String Double
powCheck calc_one calc_two = case calc_one of
                                Left msg -> Left msg
                                Right one -> case calc_two of
                                    Left msg -> Left msg
                                    Right two -> Right (one ** two)

divCheck :: Either String Double -> Either String Double -> Either String Double
divCheck calc_one calc_two = case calc_one of
                                Left msg -> Left msg
                                Right one -> case calc_two of
                                    Left msg -> Left msg
                                    Right two -> case two of
                                        0.0 -> Left "Division by Zero"
                                        _ -> Right (one /two)

multCheck :: Either String Double -> Either String Double -> Either String Double
multCheck calc_one calc_two = case calc_one of
                                Left msg -> Left msg
                                Right one -> case calc_two of
                                    Left msg -> Left msg
                                    Right two  -> Right (one * two)

restCheck :: Either String Double -> Either String Double -> Either String Double
restCheck calc_one calc_two = case calc_one of
                                Left msg -> Left msg
                                Right one -> case calc_two of
                                    Left msg -> Left msg
                                    Right two -> Right (one - two)

sumCheck :: Either String Double -> Either String Double -> Either String Double
sumCheck calc_one calc_two = case calc_one of
                                Left msg -> Left msg
                                Right one -> case calc_two of
                                    Left msg -> Left msg
                                    Right two -> Right (one + two)

unCheck :: Operator -> Either String Double -> Either String Double
unCheck op calc_one = case calc_one of
                    Left msg -> Left msg
                    Right one -> if op == Sum then Right one
                    else Right (-one)

calculate :: Abstract -> Either String Double
calculate (SumOp op leftPart rightPart) = let calc_one = calculate leftPart in
                                        let calc_two = calculate rightPart in
                                        sumCheck calc_one calc_two
calculate (RestOp op leftPart rightPart) = let calc_one = calculate leftPart in
                                        let calc_two = calculate rightPart in
                                        restCheck calc_one calc_two
calculate (MultOp op leftPart rightPart) = let calc_one = calculate leftPart in
                                        let calc_two = calculate rightPart in
                                        multCheck calc_one calc_two
calculate (DivOp op leftPart rightPart) = let calc_one = calculate leftPart in
                                        let calc_two = calculate rightPart in
                                        divCheck calc_one calc_two
calculate (PowOp op leftPart rightPart) = let calc_one = calculate leftPart in
                                        let calc_two = calculate rightPart in
                                        powCheck calc_one calc_two
calculate (UnOp op part) = let calc_one = calculate part in unCheck op calc_one
calculate (Num num) = Right num
calculate (Error str) = Left str