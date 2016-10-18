open Calc.Interpreter

printf "%A " (evaluate "a=true;b=if a then true else false;if b then 1 else 2;")

open System
Console.ReadKey() |> ignore