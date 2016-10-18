open Calc.Lexer

open Yard.Generators.Common.AST
open Yard.Generators.RNGLR.Parser

open System.Collections.Generic


open Calc.AST

let evaluate (code: string) =
    let lexbuf = Microsoft.FSharp.Text.Lexing.LexBuffer<_>.FromString code
    let allTokens = 
        seq
            {
                while not lexbuf.IsPastEndOfStream do yield token lexbuf
            }

    let translateArgs = {
        tokenToRange = fun x -> 0UL,0UL
        zeroPosition = 0UL
        clearAST = false
        filterEpsilons = true
    }
    
    let tree: list<list<Stmt>>=
        match Calc.Parser.buildAst allTokens with
        | Success (sppf, t, d) -> Calc.Parser.translate translateArgs sppf d 
        | Error (pos,errs,msg,dbg,_) -> failwithf "Error: %A    %A \n %A"  pos errs msg

    let variables = new Dictionary<Var, Value>()
   
    let rec evalAriphmic t =
        match t with
        | BinOp(o, e1, e2)         -> 
            let op = 
                match o with 
                | Pow   -> ( **)
                | Plus  -> (+)
                | Mult  -> (*)
                | Div   -> (/)
                | Minus -> (-)
                | _     -> failwith "Type mismatch"
            (op) <| evalAriphmic e1 <| evalAriphmic e2            
        | ConditionStmt(c, e1, e2)  -> if evalBool c then evalAriphmic e1 else evalAriphmic e2
        | Num n                     -> n
        | EVar v                    -> 
            let value = variables.[v]
            match value with
            | Number n-> n
            | Boolean b -> failwith "type mismatch"

    and evalBool t =
        match t with
        | BoolConst b        -> b
        | BinOp(And, e1, e2) -> evalBool e1 && evalBool e2
        | BinOp(Or, e1, e2)  -> evalBool e1 || evalBool e2     
        | UnaryOp(Not, e)    -> not(evalBool e)

        | CompOp(o, e1, e2) -> 
            let op = 
                match o with 
                | Less      -> (<)
                | Greater   -> (>)
                | LessEq    -> (<=)
                | GreaterEq -> (>=)
                | Eq        -> (=)
                | _         -> failwith "Type mismatch"
            (op) <| evalAriphmic e1 <| evalAriphmic e2
        | ConditionStmt(c, e1, e2)  -> if evalBool c then evalBool e1 else evalBool e2     
        | EVar v -> 
            let value = variables.[v]
            match value with
            | Number n-> failwith "type mismatch"
            | Boolean b -> b 
     
    let evalStmt s =
        let v, e, t = s
        let result =
            match t with
            | BooleanT  -> Boolean (evalBool e)
            | AriphmicT -> Number (evalAriphmic e)
        if v <> null then
            variables.[v] <- result           
        result
    let (Number r) = (List.map evalStmt tree.Head |> List.rev |> List.head)
    r
printf "%A " (evaluate "a=true;b=if a then true else false;if b then 1 else 2;")

open System
Console.ReadKey() |> ignore