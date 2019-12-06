
[<EntryPoint>]
let main args =
    let ParsePair (s:string) = let a = Seq.toArray (s.Split ')') in (Array.get a 1),(Array.get a 0)
    let input = args.[0] |> System.IO.File.ReadAllLines |> Seq.map ParsePair |> Seq.toList
    let orbitTree = Map.ofList input

    let OrbitPath s =
        let rec PathSeq s = seq {
            yield s
            if s <> "COM" then yield! PathSeq orbitTree.[s]
        }
        Seq.toArray (PathSeq s)

    let youPath = OrbitPath "YOU"
    let sanPath = OrbitPath "SAN"

    let SuffixLength a b =
        let a',b' = Array.rev a, Array.rev b
        let rec PrefixLength i =
            if (Array.get a' i) <> (Array.get b' i) then 0
            else 1 + (PrefixLength (i+1))
        PrefixLength 0

    let suffixLength = SuffixLength youPath sanPath
    let youLength = youPath.Length - suffixLength
    let sanLength = sanPath.Length - suffixLength

    let transfers = youLength + sanLength - 2
    printfn "%i" transfers
    0
