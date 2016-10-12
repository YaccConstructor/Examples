module Calc.AST

open System.Collections.Generic

type Var = string

let vars = new Dictionary<string, float>()

type Op = 
    | Pow
    | Plus
    | Mult
    | Div
    | Minus

type Expr =
    | Num of float
    | EVar of Var
    | BinOp of Op*Expr*Expr*float

type Stmt = 
    | EqStmt of Var*Expr
    | SingleExpr of Expr

type program = List<Stmt>