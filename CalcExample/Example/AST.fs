module Calc.AST

open System.Collections.Generic

type Var = string

let mutable returnVal = nan

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

let getValue e = match e with
   | Num num -> num
   | BinOp (op,l,r,result) -> result
   | EVar (name) -> vars.[name]

let calcFunc l (op, r) =
   let operator = match op with
   | Plus  -> (+)
   | Minus -> (-)
   | Mult  -> (*)
   | Div   -> (/)
   | Pow   -> ( ** )
   BinOp(op,l,r, operator (getValue l) (getValue r))