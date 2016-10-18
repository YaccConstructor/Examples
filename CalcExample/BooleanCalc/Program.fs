open Calc.Lexer

open Yard.Generators.Common.AST
open Yard.Generators.RNGLR.Parser

open System.Collections.Generic


open Calc.AST

let main (inputFile: string) = 
    use reader = new System.IO.StreamReader(inputFile)
    let lexbuf = Microsoft.FSharp.Text.Lexing.LexBuffer<_>.FromTextReader reader
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
    
    let tree:list<list<Stmt>> =
        match Calc.Parser.buildAst allTokens with
        | Success (sppf, t, d) -> Calc.Parser.translate translateArgs sppf d 
//        | Error (pos,errs,msg,dbg,_) -> failwithf "Error: %A    %A \n %A"  pos errs msg

    let variables = new Dictionary<Var, Variable>()
    
    let rec evalAriphmic t =
       match t with
        | BinOp(Pow, e1, e2)   -> evalAriphmic e1 ** evalAriphmic e2
        | BinOp(Plus, e1, e2)  -> evalAriphmic e1 + evalAriphmic e2
        | BinOp(Mult, e1, e2)  -> evalAriphmic e1 * evalAriphmic e2
        | BinOp(Div, e1, e2)   -> evalAriphmic e1 / evalAriphmic e2
        | BinOp(Minus, e1, e2) -> evalAriphmic e1 - evalAriphmic e2 
        | Num n                -> n
        | EVar v               -> 
            let value = variables.[v]
            match value with
            | Number n-> n
            | Boolean b -> float 0 //Throw error here  

    let rec evalBool t =
        match t with
        | BoolConst b               -> b
        | BinOp(And, e1, e2)        -> evalBool e1 && evalBool e2
        | BinOp(Or, e1, e2)         -> evalBool e1 || evalBool e2     
        | UnaryOp(Not, e)           -> not(evalBool e)
        | CompOp(Less, e1, e2)      -> evalAriphmic e1 < evalAriphmic e2
        | CompOp(Greater, e1, e2)   -> evalAriphmic e1 > evalAriphmic e2
        | CompOp(LessEq, e1, e2)    -> evalAriphmic e1 <= evalAriphmic e2  
        | CompOp(GreaterEq, e1, e2) -> evalAriphmic e1 >= evalAriphmic e2
        | CompOp(Eq, e1, e2)        -> evalAriphmic e1 = evalAriphmic e2
        | EVar v               -> 
            let value = variables.[v]
            match value with
            | Number n-> false //Throw error here 
            | Boolean b -> b 

     
    let evalStmt s =
        let v, e, t = s

        let result =
            match t with
            | BooleanT  -> Boolean (evalBool e)
            | AriphmicT -> Number (evalAriphmic e)
        variables.[v] <- result
        printf "%s = %A\n" v result
    List.map evalStmt tree.Head |> ignore
main @"..\..\input"

open System
Console.ReadKey() |> ignore