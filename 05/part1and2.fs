
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

let rec ExecuteCmd (program:int array) input pc =
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
        ExecuteCmd program input (pc+4)

    let Input() =
        SetParam 1 (List.head input)
        ExecuteCmd program (List.tail input) (pc+2)

    let Output() =
        printf "%i " (GetParam 1)
        ExecuteCmd program input (pc+2)

    let JumpIfTrue() =
        if (GetParam 1) > 0 then
            ExecuteCmd program input (GetParam 2)
        else
            ExecuteCmd program input (pc+3)

    let JumpIfFalse() =
        if (GetParam 1) = 0 then
            ExecuteCmd program input (GetParam 2)
        else
            ExecuteCmd program input (pc+3)

    let LessThan() =
        if (GetParam 1) < (GetParam 2) then
            SetParam 3 1
        else
            SetParam 3 0
        ExecuteCmd program input (pc+4)

    let Equals() =
        if (GetParam 1) = (GetParam 2) then
            SetParam 3 1
        else
            SetParam 3 0
        ExecuteCmd program input (pc+4)

    match opc with
    |  1 -> AluOpc (+)
    |  2 -> AluOpc (*)
    |  3 -> Input()
    |  4 -> Output()
    |  5 -> JumpIfTrue()
    |  6 -> JumpIfFalse()
    |  7 -> LessThan()
    |  8 -> Equals()
    | 99 -> printfn "(halt)"
    | _  -> failwith (sprintf "unknown opcode '%i' at pc=%i"  opc pc)

// call with "mono part1and2.exe input 0" for part 1
// call with "mono part1and2.exe input 5" for part 2
[<EntryPoint>]
let main args =
    let input =
        args.[0] |> System.IO.File.ReadAllText
    let program =
        input.Split ',' |> Seq.map int |> Seq.toArray
    ExecuteCmd program [(int args.[1])] 0
    0
