# Functional EvalExpr
The goal of this project is to implement a functional EvalExpr, which takes a string as parameter, which represents a mathematical expression, evaluates this expression and
returns the result as an integer
# Project
Your program must handle float numbers, parentheses and the following binary operators:

* sum (+) and difference (-)
* product (*) and division (\)
* power (^)
The list above is sorted by precedence, from lower to higher.
# Usage
```
>> ./funEvalExpr "3 + 5.34"
8.34
>>./funEvalExpr “(0.345+ 5 )*( -2-1) / 3"
-5.35
```