
# 2 "EbnfIso.yrd.fs"
module Ebnf.Parser
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.Common.AST
type Token =
    | RNGLR_EOF of (string)
    | SECTION_HEADER of (string)
    | SUBSECTION_HEADER of (string)
    | VARIABLE_LINE of (string)

let genLiteral (str : string) (data : string) =
    match str.ToLower() with
    | x -> None
let tokenData = function
    | RNGLR_EOF x -> box x
    | SECTION_HEADER x -> box x
    | SUBSECTION_HEADER x -> box x
    | VARIABLE_LINE x -> box x

let numToString = function
    | 0 -> "error"
    | 1 -> "file_content"
    | 2 -> "list_of_body_units"
    | 3 -> "list_of_subsections"
    | 4 -> "section"
    | 5 -> "section_body"
    | 6 -> "subsection"
    | 7 -> "yard_opt_1"
    | 8 -> "yard_opt_2"
    | 9 -> "yard_opt_3"
    | 10 -> "yard_opt_4"
    | 11 -> "yard_opt_5"
    | 12 -> "yard_some_1"
    | 13 -> "yard_some_2"
    | 14 -> "yard_some_3"
    | 15 -> "yard_start_rule"
    | 16 -> "RNGLR_EOF"
    | 17 -> "SECTION_HEADER"
    | 18 -> "SUBSECTION_HEADER"
    | 19 -> "VARIABLE_LINE"
    | _ -> ""

let tokenToNumber = function
    | RNGLR_EOF _ -> 16
    | SECTION_HEADER _ -> 17
    | SUBSECTION_HEADER _ -> 18
    | VARIABLE_LINE _ -> 19

let isLiteral = function
    | RNGLR_EOF _ -> false
    | SECTION_HEADER _ -> false
    | SUBSECTION_HEADER _ -> false
    | VARIABLE_LINE _ -> false

let getLiteralNames = []
let mutable private cur = 0
let leftSide = [|1; 15; 12; 12; 7; 7; 4; 8; 8; 5; 9; 9; 10; 10; 2; 13; 13; 3; 14; 14; 6; 11; 11|]
let private rules = [|7; 1; 4; 4; 12; 12; 17; 8; 5; 9; 10; 2; 3; 13; 19; 19; 13; 14; 6; 6; 14; 18; 11; 2|]
let private rulesStart = [|0; 1; 2; 3; 5; 5; 6; 8; 8; 9; 11; 11; 12; 12; 13; 14; 15; 17; 18; 19; 21; 23; 23; 24|]
let startRule = 1

let acceptEmptyInput = true

let defaultAstToDot =
    (fun (tree : Yard.Generators.Common.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let otherAstToDot =
    (fun (tree : Yard.Generators.RNGLR.OtherSPPF.OtherTree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 2; 20; 21; 4; 3; 5; 6; 7; 8; 15; 16; 9; 10; 18; 19; 12; 11; 13; 14; 17|]
let private small_gotos =
        [|5; 65536; 262145; 458754; 786435; 1114116; 131075; 262145; 786437; 1114116; 262150; 131078; 327687; 524296; 589833; 851978; 1245195; 524293; 196620; 393229; 655374; 917519; 1179664; 655363; 393229; 917521; 1179664; 786436; 131090; 720915; 851978; 1245195; 1048578; 851988; 1245195|]
let gotos = Array.zeroCreate 22
for i = 0 to 21 do
        gotos.[i] <- Array.zeroCreate 20
cur <- 0
while cur < small_gotos.Length do
    let i = small_gotos.[cur] >>> 16
    let length = small_gotos.[cur] &&& 65535
    cur <- cur + 1
    for k = 0 to length-1 do
        let j = small_gotos.[cur + k] >>> 16
        let x = small_gotos.[cur + k] &&& 65535
        gotos.[i].[j] <- lists_gotos.[x]
    cur <- cur + length
let private lists_reduces = [|[|2,1|]; [|3,2|]; [|6,1|]; [|11,1|]; [|8,1|]; [|6,2|]; [|9,1|]; [|13,1|]; [|18,1|]; [|19,2|]; [|20,1|]; [|22,1|]; [|20,2|]; [|14,1|]; [|15,1|]; [|16,2|]; [|9,2|]; [|17,1|]; [|0,1|]; [|5,1|]|]
let private small_reduces =
        [|131073; 1048576; 196609; 1048577; 262146; 1048578; 1114114; 327683; 1048579; 1114115; 1179651; 393218; 1048580; 1114116; 458754; 1048581; 1114117; 524290; 1048582; 1114118; 589826; 1048583; 1114119; 655362; 1048584; 1114120; 720898; 1048585; 1114121; 786435; 1048586; 1114122; 1179658; 851971; 1048587; 1114123; 1179659; 917507; 1048588; 1114124; 1179660; 983043; 1048589; 1114125; 1179661; 1048579; 1048590; 1114126; 1179662; 1114115; 1048591; 1114127; 1179663; 1179650; 1048592; 1114128; 1245186; 1048593; 1114129; 1310721; 1048594; 1376257; 1048595|]
let reduces = Array.zeroCreate 22
for i = 0 to 21 do
        reduces.[i] <- Array.zeroCreate 20
cur <- 0
while cur < small_reduces.Length do
    let i = small_reduces.[cur] >>> 16
    let length = small_reduces.[cur] &&& 65535
    cur <- cur + 1
    for k = 0 to length-1 do
        let j = small_reduces.[cur + k] >>> 16
        let x = small_reduces.[cur + k] &&& 65535
        reduces.[i].[j] <- lists_reduces.[x]
    cur <- cur + length
let private lists_zeroReduces = [|[|4; 1; 0|]; [|10; 9; 8; 7|]; [|10|]; [|12|]; [|21|]|]
let private small_zeroReduces =
        [|1; 1048576; 262147; 1048577; 1114113; 1179650; 524290; 1048579; 1114115; 786435; 1048580; 1114116; 1179652|]
let zeroReduces = Array.zeroCreate 22
for i = 0 to 21 do
        zeroReduces.[i] <- Array.zeroCreate 20
cur <- 0
while cur < small_zeroReduces.Length do
    let i = small_zeroReduces.[cur] >>> 16
    let length = small_zeroReduces.[cur] &&& 65535
    cur <- cur + 1
    for k = 0 to length-1 do
        let j = small_zeroReduces.[cur + k] >>> 16
        let x = small_zeroReduces.[cur + k] &&& 65535
        zeroReduces.[i].[j] <- lists_zeroReduces.[x]
    cur <- cur + length
let private small_acc = [1; 0]
let private accStates = Array.zeroCreate 22
for i = 0 to 21 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 16
let errorIndex = 0
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAstAbstract : (seq<int*array<'TokenType*int>> -> ParseResult<Token>) =
    buildAstAbstract<Token> parserSource

let buildAst : (seq<'TokenType> -> ParseResult<Token>) =
    buildAst<Token> parserSource


let _rnglr_epsilons : Tree<Token>[] = [|new Tree<_>(null,box (new AST(new Family(23, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(0, new Nodes([|box (new AST(new Family(4, new Nodes([||])), null))|])), null)), null); null; null; null; new Tree<_>(null,box (new AST(new Family(9, new Nodes([|box (new AST(new Family(10, new Nodes([||])), null)); box (new AST(new Family(12, new Nodes([||])), null))|])), null)), null); null; new Tree<_>(null,box (new AST(new Family(4, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(7, new Nodes([||])), [|new Family(8, new Nodes([|box (new AST(new Family(9, new Nodes([|box (new AST(new Family(10, new Nodes([||])), null)); box (new AST(new Family(12, new Nodes([||])), null))|])), null))|]))|])), null); new Tree<_>(null,box (new AST(new Family(10, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(12, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(21, new Nodes([||])), null)), null); null; null; null; new Tree<_>(null,box (new AST(new Family(1, new Nodes([|box (new AST(new Family(0, new Nodes([|box (new AST(new Family(4, new Nodes([||])), null))|])), null))|])), null)), null)|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|new Tree<_>(null,box (new AST(new Family(23, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(0, new Nodes([|box (new AST(new Family(4, new Nodes([||])), null))|])), null)), null); null; null; null; new Tree<_>(null,box (new AST(new Family(9, new Nodes([|box (new AST(new Family(10, new Nodes([||])), null)); box (new AST(new Family(12, new Nodes([||])), null))|])), null)), null); null; new Tree<_>(null,box (new AST(new Family(4, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(7, new Nodes([||])), [|new Family(8, new Nodes([|box (new AST(new Family(9, new Nodes([|box (new AST(new Family(10, new Nodes([||])), null)); box (new AST(new Family(12, new Nodes([||])), null))|])), null))|]))|])), null); new Tree<_>(null,box (new AST(new Family(10, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(12, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(21, new Nodes([||])), null)), null); null; null; null; new Tree<_>(null,box (new AST(new Family(1, new Nodes([|box (new AST(new Family(0, new Nodes([|box (new AST(new Family(4, new Nodes([||])), null))|])), null))|])), null)), null)|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_error * '_rnglr_type_file_content * '_rnglr_type_list_of_body_units * '_rnglr_type_list_of_subsections * '_rnglr_type_section * '_rnglr_type_section_body * '_rnglr_type_subsection * '_rnglr_type_yard_opt_1 * '_rnglr_type_yard_opt_2 * '_rnglr_type_yard_opt_3 * '_rnglr_type_yard_opt_4 * '_rnglr_type_yard_opt_5 * '_rnglr_type_yard_some_1 * '_rnglr_type_yard_some_2 * '_rnglr_type_yard_some_3 * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_opt_1) 
             |> List.iter (fun (S1) -> 
              _rnglr_cycle_res := (
                
# 15 "EbnfIso.yrd"
                                 S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 15 "EbnfIso.yrd"
               : '_rnglr_type_file_content) 
# 160 "EbnfIso.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_file_content) 
            )
# 15 "EbnfIso.yrd"
               : '_rnglr_type_yard_start_rule) 
# 170 "EbnfIso.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_section) 
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 15 "EbnfIso.yrd"
                                 [yard_elem]
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 15 "EbnfIso.yrd"
               : '_rnglr_type_yard_some_1) 
# 190 "EbnfIso.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_section) 
             |> List.iter (fun (yard_head) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_some_1) 
               |> List.iter (fun (yard_tail) -> 
                _rnglr_cycle_res := (
                  
# 15 "EbnfIso.yrd"
                                   yard_head::yard_tail
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 15 "EbnfIso.yrd"
               : '_rnglr_type_yard_some_1) 
# 212 "EbnfIso.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 15 "EbnfIso.yrd"
                               None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 15 "EbnfIso.yrd"
               : '_rnglr_type_yard_opt_1) 
# 230 "EbnfIso.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (
              let _rnglr_cycle_res = ref []
              ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_some_1) 
               |> List.iter (fun (S1) -> 
                _rnglr_cycle_res := (
                  
# 15 "EbnfIso.yrd"
                                   S1
                    )::!_rnglr_cycle_res )
              !_rnglr_cycle_res
            ) |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 15 "EbnfIso.yrd"
                                 Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 15 "EbnfIso.yrd"
               : '_rnglr_type_yard_opt_1) 
# 259 "EbnfIso.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with SECTION_HEADER _rnglr_val -> [_rnglr_val] | a -> failwith "SECTION_HEADER expected, but %A found" a )
             |> List.iter (fun (S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_opt_2) 
               |> List.iter (fun (S2) -> 
                _rnglr_cycle_res := (
                  
# 17 "EbnfIso.yrd"
                            S1, S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 17 "EbnfIso.yrd"
               : '_rnglr_type_section) 
# 281 "EbnfIso.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 17 "EbnfIso.yrd"
                                         None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 17 "EbnfIso.yrd"
               : '_rnglr_type_yard_opt_2) 
# 299 "EbnfIso.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (
              let _rnglr_cycle_res = ref []
              ((unbox _rnglr_children.[0]) : '_rnglr_type_section_body) 
               |> List.iter (fun (S1) -> 
                _rnglr_cycle_res := (
                  
# 17 "EbnfIso.yrd"
                                             S1
                    )::!_rnglr_cycle_res )
              !_rnglr_cycle_res
            ) |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 17 "EbnfIso.yrd"
                                           Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 17 "EbnfIso.yrd"
               : '_rnglr_type_yard_opt_2) 
# 328 "EbnfIso.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_opt_3) 
             |> List.iter (fun (S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_opt_4) 
               |> List.iter (fun (S2) -> 
                _rnglr_cycle_res := (
                  
# 18 "EbnfIso.yrd"
                                   S1, S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 18 "EbnfIso.yrd"
               : '_rnglr_type_section_body) 
# 350 "EbnfIso.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 18 "EbnfIso.yrd"
                               None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 18 "EbnfIso.yrd"
               : '_rnglr_type_yard_opt_3) 
# 368 "EbnfIso.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (
              let _rnglr_cycle_res = ref []
              ((unbox _rnglr_children.[0]) : '_rnglr_type_list_of_body_units) 
               |> List.iter (fun (S1) -> 
                _rnglr_cycle_res := (
                  
# 18 "EbnfIso.yrd"
                                   S1
                    )::!_rnglr_cycle_res )
              !_rnglr_cycle_res
            ) |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 18 "EbnfIso.yrd"
                                 Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 18 "EbnfIso.yrd"
               : '_rnglr_type_yard_opt_3) 
# 397 "EbnfIso.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 18 "EbnfIso.yrd"
                                                      None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 18 "EbnfIso.yrd"
               : '_rnglr_type_yard_opt_4) 
# 415 "EbnfIso.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (
              let _rnglr_cycle_res = ref []
              ((unbox _rnglr_children.[0]) : '_rnglr_type_list_of_subsections) 
               |> List.iter (fun (S1) -> 
                _rnglr_cycle_res := (
                  
# 18 "EbnfIso.yrd"
                                                          S1
                    )::!_rnglr_cycle_res )
              !_rnglr_cycle_res
            ) |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 18 "EbnfIso.yrd"
                                                        Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 18 "EbnfIso.yrd"
               : '_rnglr_type_yard_opt_4) 
# 444 "EbnfIso.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_some_2) 
             |> List.iter (fun (S1) -> 
              _rnglr_cycle_res := (
                
# 20 "EbnfIso.yrd"
                                     S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 20 "EbnfIso.yrd"
               : '_rnglr_type_list_of_body_units) 
# 464 "EbnfIso.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with VARIABLE_LINE _rnglr_val -> [_rnglr_val] | a -> failwith "VARIABLE_LINE expected, but %A found" a )
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 20 "EbnfIso.yrd"
                                     [yard_elem]
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 20 "EbnfIso.yrd"
               : '_rnglr_type_yard_some_2) 
# 484 "EbnfIso.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with VARIABLE_LINE _rnglr_val -> [_rnglr_val] | a -> failwith "VARIABLE_LINE expected, but %A found" a )
             |> List.iter (fun (yard_head) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_some_2) 
               |> List.iter (fun (yard_tail) -> 
                _rnglr_cycle_res := (
                  
# 20 "EbnfIso.yrd"
                                       yard_head::yard_tail
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 20 "EbnfIso.yrd"
               : '_rnglr_type_yard_some_2) 
# 506 "EbnfIso.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_yard_some_3) 
             |> List.iter (fun (S1) -> 
              _rnglr_cycle_res := (
                
# 22 "EbnfIso.yrd"
                                      S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 22 "EbnfIso.yrd"
               : '_rnglr_type_list_of_subsections) 
# 526 "EbnfIso.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_subsection) 
             |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 22 "EbnfIso.yrd"
                                      [yard_elem]
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 22 "EbnfIso.yrd"
               : '_rnglr_type_yard_some_3) 
# 546 "EbnfIso.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_subsection) 
             |> List.iter (fun (yard_head) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_some_3) 
               |> List.iter (fun (yard_tail) -> 
                _rnglr_cycle_res := (
                  
# 22 "EbnfIso.yrd"
                                        yard_head::yard_tail
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 22 "EbnfIso.yrd"
               : '_rnglr_type_yard_some_3) 
# 568 "EbnfIso.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with SUBSECTION_HEADER _rnglr_val -> [_rnglr_val] | a -> failwith "SUBSECTION_HEADER expected, but %A found" a )
             |> List.iter (fun (S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_yard_opt_5) 
               |> List.iter (fun (S2) -> 
                _rnglr_cycle_res := (
                  
# 23 "EbnfIso.yrd"
                               S1, S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 23 "EbnfIso.yrd"
               : '_rnglr_type_subsection) 
# 590 "EbnfIso.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 23 "EbnfIso.yrd"
                                               None
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 23 "EbnfIso.yrd"
               : '_rnglr_type_yard_opt_5) 
# 608 "EbnfIso.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (
              let _rnglr_cycle_res = ref []
              ((unbox _rnglr_children.[0]) : '_rnglr_type_list_of_body_units) 
               |> List.iter (fun (S1) -> 
                _rnglr_cycle_res := (
                  
# 23 "EbnfIso.yrd"
                                                   S1
                    )::!_rnglr_cycle_res )
              !_rnglr_cycle_res
            ) |> List.iter (fun (yard_elem) -> 
              _rnglr_cycle_res := (
                
# 23 "EbnfIso.yrd"
                                                 Some(yard_elem)
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 23 "EbnfIso.yrd"
               : '_rnglr_type_yard_opt_5) 
# 637 "EbnfIso.yrd.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (uint64 * uint64)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

              parserRange
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_error) 
# 655 "EbnfIso.yrd.fs"
      );
  |] , [|
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_error)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_file_content)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_list_of_body_units)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_list_of_subsections)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_section)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_section_body)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_subsection)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_1)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_2)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_3)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_4)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_opt_5)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_some_1)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_some_2)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_some_3)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) (dict : _ ) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST dict) : '_rnglr_type_yard_start_rule
