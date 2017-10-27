module Calc.AST

open System.Collections.Generic

type Var = string

let mutable returnVal = nan

let vars = new Dictionary<string, float>()

type Op = 
    | Sin
    | Cos
    | Abs
    | Pow
    | Plus
    | Mult
    | Div
    | Minus
    | Negate
    | Knuth of int

type Expr =
    | Num of float
    | EVar of Var
    | UnOp of Op*Expr*float
    | BinOp of Op*Expr*Expr*float

type Stmt = 
    | EqStmt of Var*Expr
    | SingleExpr of Expr

type result = list<Stmt>*Dictionary<string,float>*float //Statements, variables, return value

let getLength (l : list<string>) = l.Length + 1

let getValue e = match e with
   | Num num -> num
   | BinOp (op,l,r,result) -> result
   | UnOp (op,e,result) -> result
   | EVar (name) -> vars.[name]

let calcFuncLeft l (op, r) =
   let operator = match op with
   | Plus  -> (+)
   | Minus -> (-)
   | Mult  -> (*)
   | Div   -> (/)
   BinOp(op, l, r, operator (getValue l) (getValue r))

let rec f (t : int) (ll : float) (rr : int) = 
    match (rr, t) with
    | _, _ when rr < 0 || t < 0 -> failwithf "Wrong args | int Overflow"
    | 0, _ -> 1.0
    | 1, _ -> ll
    | _, 1 -> ll ** (float rr)
    | _, _ -> f (t - 1) ll (int (f t ll (rr - 1)))

let calcFuncRight l (r, op) =
   let operator = match op with
   | Pow   -> ( ** )
   | Knuth(times) -> fun x y -> (f times) x (int y)
   BinOp(op, r, l, operator (getValue r) (getValue l))

let calcFuncUn op e =
   let operator = match op with
   | Sin -> (sin)
   | Cos -> (cos)
   | Abs -> (abs)
   | Negate -> ((-) 0.0)
   UnOp(op, e, operator (getValue e))