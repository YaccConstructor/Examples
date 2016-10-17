module BoolCalc.Main

open BoolCalc.Lexer
open BoolCalc.AST
open Yard.Generators.Common.AST
open Yard.Generators.RNGLR.Parser

let executeB(code) = 
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
    
    let tree:list<result> =
        match BoolCalc.Parser.buildAst allTokens with
        | Success (sppf, t, d) -> BoolCalc.Parser.translate translateArgs sppf d 
        | Error (pos,errs,msg,dbg,_) -> failwithf "Error: %A    %A \n %A"  pos errs msg

    tree.[0]

let tree: result = executeB("3+2;")
printfn "%A" tree