namespace BooleanCalcTest

open NUnit.Framework
open Calc.Interpreter

[<TestFixture>]
type TestClass() =      
    [<Test>]
    member this.simpleExpressionTest() = 
        Assert.AreEqual(evaluate "1 + 2;", 3)

    [<Test>]
    member this.longExpressionTest() = 
        Assert.AreEqual(evaluate "1 + 2 * 3 - 4 * (1 + 2) + 3 + 4;", 2)

    [<Test>]
    member this.variablesTest() = 
        Assert.AreEqual(evaluate "x = 1 + 2; y = 2 + 3; x * y;", 15)

    [<Test>]
    member this.conditionStatementTest() = 
        Assert.AreEqual(evaluate "if 3>5 then 0 else 1;", 1)