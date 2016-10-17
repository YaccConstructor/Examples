namespace ExampleTesting

open NUnit.Framework
open Calc.Main
open System.IO

[<TestFixture>]
type TestClass() =
    let stmts     (x,_,_) = x
    let vars      (_,x,_) = x
    let returnVal (_,_,x) = x
    
    [<Test>]
    member this.singleExprTest() = 
        Assert.AreEqual(returnVal(execute "0;"), 0)
        Assert.AreEqual(returnVal(execute "2**4;"), 16)
        Assert.AreEqual(returnVal(execute "x=-6;"), -6)
   
    [<Test>]
    member this.orderExprTest() = 
        Assert.AreEqual(returnVal(execute "2+2*2;"), 6)
        Assert.AreEqual(returnVal(execute "(2+2)*2;"), 8)
        Assert.AreEqual(returnVal(execute "(3*4/3) + (2+8)/5;"), 6)
        Assert.AreEqual(returnVal(execute "3/3+100*0+(12+12)*0;"), 1)
    
    [<Test>]
    member this.variablesTest() =
        let code = "x = 2;\
                    x = x + 2;\
                    y = x * 2;\
                    z = y + x;\
                    v = 0;
                    "
        let v = vars(execute code)
        Assert.AreEqual(v.["x"], 4)
        Assert.AreEqual(v.["y"], 8)
        Assert.AreEqual(v.["z"], 12)
        Assert.AreEqual(v.["v"], 0)
    
    [<Test>]
    member this.whiteSpaceTest() = 
        let code = "z=2+2;\n\n x=5+40;v   =   2;"
        let v = vars(execute code)
        Assert.AreEqual(v.["z"], 4)
        Assert.AreEqual(v.["x"],45)
        Assert.AreEqual(v.["v"],2)
