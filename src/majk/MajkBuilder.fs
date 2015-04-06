
module Majk

open System
open System.Threading.Tasks


type LazyTask<'T> (f : unit -> Task<'T> )  = 
    let _source = new TaskCompletionSource<'T>()
    let mutable _task = None
    member x.Source = _source.Task
    member x.Start() =
        match _task with
        |None -> 
            let task = f()
            _task <-  Some task

            async {
                try
                    let! v = task |> Async.AwaitTask
                    _source.SetResult v
                with e ->
                    _source.SetException e
            }
            |> Async.Start
               
        |_ -> ()
        _source.Task


type MajkBuilder() = 
    member this.Delay(f) = f
    member this.Run(f:unit->LazyTask<'T>) = new LazyTask<_>(fun () -> f().Start()) 

    member this.Bind(ms:LazyTask<'T> list, f:'T list->LazyTask<'U>) =
        let x = async { 
            let! v = 
                ms
                |> Seq.map (fun m -> m.Start() |> Async.AwaitTask )
                |> Async.Parallel
            return! f(v |> Seq.toList).Start() |> Async.AwaitTask
         }

        new LazyTask<_>( fun () -> 
            x |> Async.StartAsTask 
        )

    member this.Bind(m:LazyTask<'T>, f:'T->LazyTask<'U>) =

        let x = async {
            let! v = m.Start() |> Async.AwaitTask
            return! f(v).Start() |> Async.AwaitTask 
        }

        new LazyTask<_>( fun () -> 
            x |> Async.StartAsTask 
        )

    member this.Return x =
        new LazyTask<_>( fun () -> async { return x } |> Async.StartAsTask)

let majk = new MajkBuilder()




