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
