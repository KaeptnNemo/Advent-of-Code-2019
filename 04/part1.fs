
let PreprocessInput (s:string) = s |> Seq.toArray |> Array.rev

let input = [| PreprocessInput "178416" ; PreprocessInput "676461" |]

let Inc (s:char array) =
    let rec IncAt n =
        s.[n] <- s.[n] + (char 1)
        if s.[n] > '9' then
            s.[n] <- '0'
            do IncAt (n+1)
    do IncAt 0
    ()

let HasTwoAdjacentSame (s:char array) =
    let rec SameAt n =
        if n > (s.Length-2) then false
        elif s.[n] = s.[n+1] then true
        else SameAt (n+1)
    SameAt 0

let IsNotDecreasing (s:char array) =
    let rec IncreasesAt n =
        if n > (s.Length-2) then false
        elif s.[n] < s.[n+1] then true
        else IncreasesAt (n+1)
    not (IncreasesAt 0)

let MeetsCriteria (s:char array) =
    (HasTwoAdjacentSame s) && (IsNotDecreasing s)

[<EntryPoint>]
let main args =
    let mutable count = 0
    do Inc input.[1]
    while input.[0] <> input.[1] do
        if (MeetsCriteria input.[0]) then
            count <- count + 1
        do Inc input.[0]

    printfn "%i" count
    0
