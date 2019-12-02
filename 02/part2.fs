
let rec Execute (pc:int) (program: int array) =
    let Opc func =
        program.[program.[pc+3]] <- func program.[program.[pc+1]] program.[program.[pc+2]]
        do Execute (pc+4) program

    match program.[pc] with
    |  1 -> do Opc (+)
    |  2 -> do Opc (*)
    | 99 -> ()
    | _  -> failwith "unknown opcode"

[<EntryPoint>]
let main args =
    let input = System.IO.File.ReadAllText args.[0]
    let program = input.Split ',' |> Seq.map int |> Seq.toArray

    let combinations = seq {
        for noun in seq { 0 .. 99 } do
            for verb in seq { 0 .. 99 } do
                let program' = Array.copy program
                program'.[1] <- noun
                program'.[2] <- verb
                do Execute 0 program'
                if program'.[0] = 19690720 then yield (noun,verb)
    }

    let noun,verb = Seq.head combinations
    printfn "%i" (100 * noun + verb)
    0
