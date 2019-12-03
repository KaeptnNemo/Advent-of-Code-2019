
let ParseDirection (s:string) =
    let dist = int s.[1..(s.Length-1)]
    match s.[0] with
    | 'U' -> 0,-dist
    | 'D' -> 0, dist
    | 'L' -> -dist,0
    | 'R' ->  dist,0
    | _   -> failwith "unknown direction"

let ParseCable (s:string) =
    s.Split ',' |> Seq.map ParseDirection

let DirectionsToLineSegments direction =
    let mutable last = 0,0
    let lineSeq = seq {
        for d in direction do
            let last' = (fst last + fst d, snd last + snd d)
            yield last,last'
            last <- last'
    }
    Seq.toArray lineSeq

let Intersetcts l1 l2 =
    let PointOnLine l p =
        let ((lx1,ly1),(lx2,ly2)) = l
        let x,y = p
        (lx1 <= x && x <= lx2 || lx2 <= x && x <= lx1) &&
        (ly1 <= y && y <= ly2 || ly2 <= y && y <= ly1)

    let ((l1x1,l1y1),(l1x2,l1y2)) = l1
    let ((l2x1,l2y1),(l2x2,l2y2)) = l2

    let l1vert = l1x1 = l1x2
    let l2vert = l2x1 = l2x1

    if l1vert = l2vert then
        (0,0)
    else
        let cross = if l1vert then (l1x1,l2y1) else (l2x1,l1y1)
        if (PointOnLine l1 cross) && (PointOnLine l2 cross) then
            cross
        else
            (0,0)

[<EntryPoint>]
let main args =
    let input      = args.[0] |> System.IO.File.ReadAllLines |> Seq.toArray
    let directions = Array.map ParseCable input
    let cables     = Array.map DirectionsToLineSegments directions

    let crosses = seq {
        for l1 in cables.[0] do
            for l2 in cables.[1] do
                yield Intersetcts l1 l2
    }

    let closest =
        crosses
        |> Seq.filter ((<>)(0,0))
        |> Seq.map (fun (x,y) -> abs x + abs y)
        |> Seq.min
    printfn "%i" closest

    0
