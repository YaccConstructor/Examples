module BoolCalc.AST

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

type LogOp = 
    | Or
    | And
    
type CompOp =
    | Equal
    | NotEqual
    | Gr
    | Ge
    | Ls
    | Le

type BoolCond = 
    | BoolConst of bool
    | NotCond of  BoolCond*bool
    | LogCond of  LogOp*BoolCond*BoolCond*bool
    | CompCond of CompOp*Expr*Expr*bool 

and Expr =
    | Num of float
    | EVar of Var
    | BinOp of Op*Expr*Expr*float
    | IfThenElse of BoolCond*Expr*Expr

type Stmt = 
    | EqStmt of Var*Expr
    | SingleExpr of Expr

type result = list<Stmt>*Dictionary<string,float>*float //Statements, variables, return value

let getBoolValue b = match b with
    | BoolConst boolean -> boolean
    | NotCond (_,res) -> res  
    | CompCond (_,_,_,res) | LogCond (_,_,_,res)
      -> res

let rec getValue e = match e with
    | Num num               -> num
    | BinOp (op,l,r,result) -> result
    | EVar (name)           -> vars.[name]
    | IfThenElse (c,e1,e2)  -> if getBoolValue c then getValue e1 else getValue e2

let calcFunc l (op, r) =
    let operator = match op with
    | Plus  -> (+)
    | Minus -> (-)
    | Mult  -> (*)
    | Div   -> (/)
    | Pow   -> ( ** )
    BinOp(op,l,r, operator (getValue l) (getValue r))

let calcLogBool l (op,r)  =
    let log = match op with
    | And -> (&&)
    | Or  -> (||)
    LogCond(op,l,r, log (getBoolValue l) (getBoolValue r))

let calcCompBool l op r =
    let comp = match op with
    | Equal    -> (=)
    | NotEqual -> (<>)
    | Gr       -> (>)
    | Ge       -> (>=)
    | Ls       -> (<)
    | Le       -> (<=)
    CompCond(op,l,r, comp (getValue l)  (getValue r))

let calcNotBool e = NotCond(e, not (getBoolValue e))
