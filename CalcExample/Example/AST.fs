module Calc.AST

type Var = string

type Op = 
    | Pow
    | Plus
    | Mult
    | Div
    | Minus

type Expr =
    | Num of float
    | EVar of Var
    | BinOp of Op*Expr*Expr
    | Stmt of Var*Expr

type program = List<Expr>

