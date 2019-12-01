
[<EntryPoint>]
let main args =
    args.[0]
    |> System.IO.File.ReadAllLines
    |> Seq.map int
    |> Seq.map (fun i -> i/3-2)
    |> Seq.fold (+) 0
    |> (printfn "%i")
    |> ignore
    0
