namespace ExampleTesting

open NUnit.Framework
open Calc.Main
open BoolCalc.Main
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

     //BoolCalc test
     [<Test>]
     member this.simpleIfThenElseTest() =
        Assert.AreEqual(returnVal(executeB "if true then 0 else 1;"), 0);
        Assert.AreEqual(returnVal(executeB "if (2+2 > 5) then 5+2 else 5+3;"), 8);
        Assert.AreEqual(returnVal(executeB "if (25 = 5**2) then 0 else 1;"), 0);
        Assert.AreEqual(returnVal(executeB "if (26 < 5**2) then 0 else 1;"), 1);
     
     [<Test>]
     member this.logIfThenElseTest() =
        //OR
        Assert.AreEqual(returnVal(executeB "if (false || false) then 1 else 0;") , 0)
        Assert.AreEqual(returnVal(executeB "if (false || true) then 1 else 0;" ) , 1)
        Assert.AreEqual(returnVal(executeB "if (true  || false) then 1 else 0;") , 1)
        Assert.AreEqual(returnVal(executeB "if (true  || true) then 1 else 0;" ) , 1)
        //AND
        Assert.AreEqual(returnVal(executeB "if (false && false) then 1 else 0;") , 0)
        Assert.AreEqual(returnVal(executeB "if (false && true) then 1 else 0;" ) , 0)
        Assert.AreEqual(returnVal(executeB "if (true  && false) then 1 else 0;") , 0)
        Assert.AreEqual(returnVal(executeB "if (true  && true) then 1 else 0;" ) , 1)

     [<Test>]
     member this.logExpIfThenElseTest() =
        Assert.AreEqual(returnVal(executeB "if ((5+3=8)&&(3>2)) then 1 else 0;") , 1)
        Assert.AreEqual(returnVal(executeB "if ((2!=2)||(2**2=5)) then 1 else 0;"),0)
        Assert.AreEqual(returnVal(executeB "if ((2!=2)||true) then 1 else 0;"),     1)

     [<Test>]
     member this.ifThenElseWithVarsTest() =
        let code = "x = 2;\
                    z = 3;
                    y = if (((x-z)=-1)||(x>z)) then z
                    else x;"
        let v = vars(executeB code)
        Assert.AreEqual(v.["y"], v.["z"])

     [<Test>]
     member this.subIfThenElseTest() =
        let code = "x=3;
                    y=2;
                    z = if (x=3) then
                           if (y!=2) then (x**3)
                           else (y**4)
                        else 0;"
        let v = vars(executeB code)
        Assert.AreEqual(v.["z"], 16)

     [<Test>]
     member this.ifThenElseAsValue() =
        let code = "x=3;
                    y=2;
                    z = y + (if (x=3) then
                               if (y!=2) then (x**3)
                               else (y**4)
                             else 0)- 16;"
        let v = vars(executeB code)
        Assert.AreEqual(v.["z"], 2)

     [<Test>]
     member this.notOpTest() =
        Assert.AreEqual(returnVal(executeB "if !true then 1 else 0;"), 0)
        Assert.AreEqual(returnVal(executeB "if !false then 1 else 0;"),1)
        Assert.AreEqual(returnVal(executeB "if !(3>2) then 1 else 0;"),0)
        Assert.AreEqual(returnVal(executeB "if (!false && !false) then 1 else 0;"),1)