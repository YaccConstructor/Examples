#I @"..\..\packages\YC.SDK\tools\"

#r @"YC.Common.dll"
#r @"YC.RNGLR.dll"
#r @"YC.YardFrontend.dll"
#r @"YC.Conversions.dll"

open Yard.Generators.RNGLR
open Yard.Frontends.YardFrontend
open Yard.Core.Conversions

module GenEBFParser =
    let gen = new RNGLR()
    let fe = new YardFrontend()
    let inAlt = new ExpandInnerAlt.ExpandInnerAlt()
    let ebnf = new ExpandEbnfStrict.ExpandEbnf()
    let meta = new ExpandMeta.ExpandMeta()

    let generate () = 
        let il = 
            fe.ParseGrammar "EbnfIso.yrd"
            |> fun il -> {il with grammar = ebnf.ConvertGrammar il.grammar}
            |> fun il -> {il with grammar = meta.ConvertGrammar il.grammar}
            |> fun il -> {il with grammar = inAlt.ConvertGrammar il.grammar}

        gen.Generate(il, true)

GenEBFParser.generate()