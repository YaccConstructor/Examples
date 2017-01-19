CalcExample
========

Hello, in this example we cover all basic usage of FsYARD.

We want to create a parser for a calculator that would be able to take multiple statements and then evaluate them one by one.


Let us define our calculator's input as: 

1. lines("statements") that are divided by semicolons.
2. statement is assignment of expression to a string variable.
3. supported operations in expression are '+', '-', '*', '/' and ' **'(power). 

Long story short, here is the sample of input:

	v = 1 + 3;
	x = v * 6 + v * 3;
	z = v ** 3 - x * 2;

The desired form of output is AST specified in `AST.fs`

Ok, now let us jump straight into the action.

First of all we need to get tokens from input. We will use bundled FsLex for this task and `Calc.fsl` file where tokens are defined. Just take a look at it.

By running `genlex.cmd` we get `CalcLexer.fs` file that is produced output of FsLex. 

Now let us dive in details with grammar definition file(`Calc.yrd`):

File starts with the **head part**. This is F# code that would be copied to generated output file. Here we are opening AST module since we use types from it in our grammar.

    {
    open Calc.AST
    }

Then we define types of our tokens in **tokens part**. Here all tokens will be of type `string`.

	tokens {
    	_ of string
	}

Then goes the **options part**, where we define parameters that we usually pass to YaccConstructor when running it from command line.

	options {
	    translate = true
	    module = "Calc.Parser"
	    infEpsPath = epsilons
	    pos = uint64
	}

Then goes the **grammar part**, the most interesting to us. 

It starts with name of the module. As you know, grammar is a set of modules in YaccConstructor. In this example, we will review the grammar that consists of only one module.

	module Main

Now we can write our rules. First rule is a starting rule and that is why we mark it with special `[<Start>]` annotation. Our calc's input are a one or more statements that are divided by semicolon, so we write it like this:

	[<Start>]
	program: (stmt SEMI)+

Now we need to define what statement is. It is a string variable equals expression. Note that `{v, e}` is an action code that will return `v` and `e` to us.

	stmt: v=VAR EQ e=expr {v, e}

To avoid duplications in code, let us create a binExpr rule that is parametrized by `operand` and `binOp`. Basically, binExpr is a list of operands that are separated by binary operations that will be folded.

	binExpr<operand binOp>: h=operand tl=(binOp operand)* {List.fold (fun l (op,r) -> BinOp(op,l,r)) h tl}
	  
Now we want to define an expression using `binExpr`:

	expr: binExpr<term termOp> 

Term operation definition. Note that we return `Plus` and `Minus` from `Calc.AST` module:	

	termOp: PLUS {Plus} | MINUS {Minus}
	
Term definition:

	term: binExpr<factor factorOp>
	     
Factor term operation definition. Again, note that we return `Mult` and `Div` from `Calc.AST` module:

	factorOp: MULT {Mult} | DIV {Div}
	
Factor term definition:

	factor: binExpr<powExpr powOp>
	
Power operation definition. Once again, note that we return `Pow` from `Calc.AST` module:

	powOp: POW {Pow}
	
Finally, power expression definition. It is a float number, variable or expression in braces:

	powExpr:
	   n=NUMBER {Num (float n)}
	   | v=VAR {EVar(v)}
	   | LBRACE e=expr RBRACE {e}

Now we have our grammar written down. Starting `genpars.cmd` gets us `Calc.yrd.fs` file that is produced output of YaccConstructor. 

Now all we have to do is get input and start translation in `Program.fs`.