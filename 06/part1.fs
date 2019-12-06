
[<EntryPoint>]
let main args =
    let ParsePair (s:string) = let a = Seq.toArray (s.Split ')') in (Array.get a 1),(Array.get a 0)
    let input = args.[0] |> System.IO.File.ReadAllLines |> Seq.map ParsePair |> Seq.toList
    let orbitTree = Map.ofList input

    let rec OrbitCount s = if s = "COM" then 0 else 1 + (OrbitCount orbitTree.[s])

    let orbitCount = input |> List.map fst |> List.map OrbitCount |> List.fold (+) 0
    printfn "%i" orbitCount
    0
