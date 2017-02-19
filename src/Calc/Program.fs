module Calc.Main

open Calc.Lexer
open Yard.Generators.Common.AST
open Yard.Generators.RNGLR.Parser
open Calc.AST
open System.Collections.Generic

let execute (code) = 
    let lexbuf = Microsoft.FSharp.Text.Lexing.LexBuffer<_>.FromString code
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
    
    let tree: list<result> =
        match Calc.Parser.buildAst allTokens with
        | Success (sppf, t, d) -> Calc.Parser.translate translateArgs sppf d 
        | Error (pos,errs,msg,dbg,_) -> failwithf "Error: %A    %A \n %A"  pos errs msg
    

    tree.[0]
    
let tree: result = execute("2+3;")
printfn "Tree %A" tree