
let Mass2Fuel m =
    let f = m / 3 - 2 in if f > 0 then f else 0

let rec Mass2FuelSeq m = seq {
    let m' = Mass2Fuel m in
        if m' <> 0 then
            yield  m'
            yield! Mass2FuelSeq m'
}

[<EntryPoint>]
let main args =
    args.[0]
    |> System.IO.File.ReadAllLines
    |> Seq.map int
    |> Seq.map Mass2FuelSeq
    |> Seq.concat
    |> Seq.fold (+) 0
    |> (printfn "%i")
    |> ignore
    0
