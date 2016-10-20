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

    [<Test>]
    member this.nestedConditionStatementsTest() = 
        Assert.AreEqual(evaluate "if if true then true else false then 1 else 0;", 1)

    [<Test>]
    member this.aLotOfNetstedIfsTest() = 
        Assert.AreEqual(evaluate "
        if 
            if true then true else false
        then
            if false then 0 else 1
        else 
            if true then 0 else 1
        ;", 1)

    [<Test>]
    member this.ifInAripmicExpressionTest() = 
        Assert.AreEqual(
            evaluate "if (((if true then 1 else 0) * 10 + (if false then 3 else 6) / 3) = 12) then 42 else 4;", 42)