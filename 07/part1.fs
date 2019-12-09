
let DigitsOf num cmd =
    let rec OnesOf num cmd = seq {
        if num > 0 then
            yield (cmd % 10)
            yield! OnesOf (num - 1) (cmd / 10)
    }
    Seq.toArray (OnesOf num cmd)

let SplitCommand cmd =
    let opc = cmd % 100
    let paramModes = DigitsOf 3 (cmd / 100)
    opc,paramModes

let rec ExecuteCmd (program:int array) input output pc =
    let opc,paramModes = SplitCommand program.[pc]

    let GetParam num =
        match paramModes.[num-1] with
        | 0 -> program.[program.[pc+num]]
        | 1 -> program.[pc+num]
        | _ -> failwith (sprintf "unknown parameter mode '%i' at pc=%i" paramModes.[num-1] pc)

    let SetParam num value =
        match paramModes.[num-1] with
        | 0 -> do program.[program.[pc+num]] <- value
        | 1 -> failwith (sprintf "store instruction with immediate mode at pc=%i" pc)
        | _ -> failwith (sprintf "unknown parameter mode '%i' at pc=%i" paramModes.[num-1] pc)

    let AluOpc func =
        SetParam 3 (func (GetParam 1) (GetParam 2))
        ExecuteCmd program input output (pc+4)

    let Input() =
        SetParam 1 (List.head input)
        ExecuteCmd program (List.tail input) output (pc+2)

    let Output() =
        ExecuteCmd program input ((GetParam 1)::output) (pc+2)

    let JumpIfTrue() =
        if (GetParam 1) > 0 then
            ExecuteCmd program input output (GetParam 2)
        else
            ExecuteCmd program input output (pc+3)

    let JumpIfFalse() =
        if (GetParam 1) = 0 then
            ExecuteCmd program input output (GetParam 2)
        else
            ExecuteCmd program input output (pc+3)

    let LessThan() =
        if (GetParam 1) < (GetParam 2) then
            SetParam 3 1
        else
            SetParam 3 0
        ExecuteCmd program input output (pc+4)

    let Equals() =
        if (GetParam 1) = (GetParam 2) then
            SetParam 3 1
        else
            SetParam 3 0
        ExecuteCmd program input output (pc+4)

    match opc with
    |  1 -> AluOpc (+)
    |  2 -> AluOpc (*)
    |  3 -> Input()
    |  4 -> Output()
    |  5 -> JumpIfTrue()
    |  6 -> JumpIfFalse()
    |  7 -> LessThan()
    |  8 -> Equals()
    | 99 -> output
    | _  -> failwith (sprintf "unknown opcode '%i' at pc=%i"  opc pc)

let ExecuteProgram program input =
    List.exactlyOne (ExecuteCmd (Array.copy program) input [] 0)

let ExecuteSettings program (settings:int array) =
    let outA = ExecuteProgram program [settings.[0]; 0]
    let outB = ExecuteProgram program [settings.[1]; outA]
    let outC = ExecuteProgram program [settings.[2]; outB]
    let outD = ExecuteProgram program [settings.[3]; outC]
    let outE = ExecuteProgram program [settings.[4]; outD]
    outE

// call with "mono part1and2.exe input 0" for part 1
// call with "mono part1and2.exe input 5" for part 2
[<EntryPoint>]
let main args =
    let input =
        args.[0] |> System.IO.File.ReadAllText
    let program =
        input.Split ',' |> Seq.map int |> Seq.toArray

    let settingsSeq = seq {
        for a in 0 .. 4 do
            for b in 0 .. 4 do
                for c in 0 .. 4 do
                    for d in 0 .. 4 do
                        for e in 0 .. 4 do
                            let s = [|a;b;c;d;e|]
                            if s = (Array.distinct s) then yield s
    }

    let maxSettings,maxValue =
        settingsSeq
        |> Seq.map (fun s -> s,ExecuteSettings program s)
        |> Seq.maxBy snd

    printfn "%A -> %i" maxSettings maxValue
    0
