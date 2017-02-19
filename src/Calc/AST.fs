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
    | Sin
    | Cos
    | Abs
    | Negate

type Expr =
    | Num of float
    | EVar of Var
    | UnaryOp of Op*Expr*float
    | BinOp of Op*Expr*Expr*float

type Stmt = 
    | EqStmt of Var*Expr
    | SingleExpr of Expr

type result = list<Stmt>*Dictionary<string,float>*float //Statements, variables, return value

let getValue e = match e with
   | Num num -> num
   | UnaryOp (op,e,result) -> result
   | BinOp (op,l,r,result) -> result
   | EVar (name) -> vars.[name]


let calcUnary op e = 
    let operator = match op with
    | Sin -> (sin)
    | Cos -> (cos)
    | Abs -> (abs)
    | Negate -> (-) 0.0
    UnaryOp(op,e,operator (getValue e))

let calcFunc l (op, r) =
   let operator = match op with
   | Plus  -> (+)
   | Minus -> (-)
   | Mult  -> (*)
   | Div   -> (/)
   | Pow   -> ( ** )
   BinOp(op,l,r, operator (getValue l) (getValue r))