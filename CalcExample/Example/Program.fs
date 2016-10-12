open Calc.Lexer

open Yard.Generators.Common.AST
open Yard.Generators.RNGLR.Parser
open Calc.AST
open System.Collections.Generic

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
        clearAST = false //try to make a tree (default is false assuming OK)
        filterEpsilons = true // filtering eps-cycles
    }
    
    let tree: list<list<Stmt * string>> =
        match Calc.Parser.buildAst allTokens with
        | Success (sppf, t, d) -> Calc.Parser.translate translateArgs sppf d 
        | Error (pos,errs,msg,dbg,_) -> failwithf "Error: %A    %A \n %A"  pos errs msg

    let variables = new Dictionary<Var, float>()
    for x in tree.Head do
        let (variable, expression) = (fst x)
        let rec traversal t =
            match t with
            | Num n                -> n
            | EVar v               -> variables.[v]
            | BinOp(Pow, e1, e2)   -> traversal e1 ** traversal e2
            | BinOp(Plus, e1, e2)  -> traversal e1 + traversal e2
            | BinOp(Mult, e1, e2)  -> traversal e1 * traversal e2
            | BinOp(Div, e1, e2)   -> traversal e1 / traversal e2
            | BinOp(Minus, e1, e2) -> traversal e1 - traversal e2
        let result = traversal expression
        variables.Add(variable, result)
        printf "%s=%f\n" variable result

main @"..\..\input"

open System
Console.ReadKey() |> ignore
