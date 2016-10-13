open Calc.Lexer

open Yard.Generators.Common.AST
open Yard.Generators.RNGLR.Parser
open Calc.AST
open System.Collections.Generic

 

open YC.PrettyPrinter.Pretty
open YC.PrettyPrinter.StructuredFormat

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
    
    let tree: list<list<Expr>> =
        match Calc.Parser.buildAst allTokens with
        | Success (sppf, t, d) -> Calc.Parser.translate translateArgs sppf d 
        | Error (pos,errs,msg,dbg,_) -> failwithf "Error: %A    %A \n %A"  pos errs msg

    let variables = new Dictionary<Var, float>()
    for x in tree.Head do
        let rec traversal t =
            match t with
            | Num n                -> n
            | EVar v               -> variables.[v]
            | Stmt (v,e)           ->
                 let r = traversal e
                 variables.[v] <- r
                 r
            | BinOp(Pow, e1, e2)   -> traversal e1 ** traversal e2
            | BinOp(Plus, e1, e2)  -> traversal e1 + traversal e2
            | BinOp(Mult, e1, e2)  -> traversal e1 * traversal e2
            | BinOp(Div, e1, e2)   -> traversal e1 / traversal e2
            | BinOp(Minus, e1, e2) -> traversal e1 - traversal e2
        let result = traversal x
        printf "%f\n" result

    let layout = 
        List.map (
            fun e ->
                let rec traversal t =
                    let frameToBrackets ex op1 isLeft =
                        // (priority, assotiative)
                        let opInfo o =
                            match o with
                            | Num  _  -> 10, false
                            | EVar _  -> 10, false
                            | Stmt _  -> 0,  false
                            | BinOp (op, _ , _) ->
                                match op with
                                | Plus  -> 1, true
                                | Minus -> 1, false
                                | Mult  -> 2, true
                                | Div   -> 2, false
                                | Pow   -> 3, false                                         
                        let (result, op2) = traversal ex
                        if opInfo op1 = opInfo op2 then
                             if not (snd <| opInfo op1) && not isLeft then bracketL result else result
                        else
                            if fst(opInfo op1) > fst(opInfo op2) then bracketL result else result
                    let opToStr op =
                        match op with
                        | Pow   -> "**"
                        | Plus  -> "+"   
                        | Mult  -> "*"   
                        | Div   -> "/"
                        | Minus -> "-"      
                    match t with
                    | Num n             -> wordL (string n), t
                    | EVar v            -> wordL v, t
                    | Stmt (v,e)        -> wordL v ^^ wordL "=" ^^ frameToBrackets e t false, t
                    | BinOp(op, e1, e2) -> frameToBrackets e1 t true ^^ wordL (opToStr op) ^^ frameToBrackets e2 t false, t                
                traversal e
        ) tree.Head |> List.map fst |> List.reduce (@@)
    let str = print 10 layout
    printfn "%s" str

main @"..\..\input"

open System
Console.ReadKey() |> ignore
