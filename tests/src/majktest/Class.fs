module MajkTest

open NUnit.Framework
open FsUnit
open Swensen.Unquote
open Majk

[<Test>]
let ``majk should not execute until forced`` () =
        let v0 = ref false

        let lt0 = majk {
            v0.Value <- true
            return 10
        }

        test <@ v0.Value = false @>
        let r = lt0.Start()
        test <@ r.Wait(500) = true @>

        test<@ r.Result = 10 @> 

        test <@ v0.Value = true @>

[<Test>]
let ``majk should compose`` () =
        let v0 = ref false
        let v1 = ref false

        let lt0 = majk {
            v0.Value <- true
            return 10
        }
        let lt1 = majk {
            let! foo = lt0
            v1.Value <- true
            return foo + 20  
        }

        test <@ v0.Value = false @>
        test <@ v1.Value = false @>

        let r = lt1.Start()
        test <@ r.Wait(500) = true @>

        test<@ r.Result = 30 @> 

        test <@ v0.Value = true @>
        test <@ v1.Value = true @>

[<Test>]
let ``majk should propogate exceptions`` () =

        let msg = "majk failed badly"
        let lt0 = majk {
            failwith msg
            return 10
        }
        let lt1 = majk {
            let! foo = lt0
            return foo + 20  
        }

        let r = lt1.Start()
        raisesWith<System.Exception> <@ r.Wait(500) @> (fun e -> <@ e.ToString().Contains msg @>)


[<Test>]
let ``We can lazily create a file`` () =
        

    let fname = "foo.txt"
    let count = ref 0
    System.IO.File.Delete fname
    let ft = majk {
        let sw = new System.IO.StreamWriter(fname)
        sw.Write("foo")
        sw.Dispose()
        count.Value <- count.Value + 1
        return fname
    }

    test <@ count.Value = 0 @>
    test <@ System.IO.File.Exists fname |> not @>

    ft.Start().Wait()
    test <@ count.Value = 1 @>
    test <@ System.IO.File.Exists fname @>

    // The body should not be run again
    ft.Start().Wait()
    test <@ count.Value = 1 @>

    test <@ System.IO.File.Exists fname @>


[<Test>]
let ``We can combine targets`` () =

    let t0 = majk { return "a" }
    let t1 = majk { return "b" }
    let t2 = majk { return "c" }

    let c = majk {

        let! v0 = t0
        let! v1 = t1
        let! v2 = t2

        return v0 + v1 + v2

    }

    c.Start().Wait()
    test <@ c.Start().Result = "abc" @>
