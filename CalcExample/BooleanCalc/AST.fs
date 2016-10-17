module Calc.AST

type Var = string

type BOp = 
    | Pow
    | Plus
    | Mult
    | Div
    | Minus
    | And
    | Or

type UOp = Not

type ExprType =
    | BooleanT
    | AriphmicT

type Variable =   
    | Number of float
    | Boolean of bool  

type Expr =
    | Num of float
    | BoolConst of bool
    | EVar of Var
    | UnaryOp of UOp*Expr
    | BinOp of BOp*Expr*Expr


type Stmt = Var*Expr * ExprType

type program = List<Stmt>

let applyOp operator operands = 
    List.fold (fun l (op,r) -> BinOp(op,l,r)) operator operands