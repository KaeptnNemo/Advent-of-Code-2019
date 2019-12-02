
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

    program.[1] <- 12
    program.[2] <-  2

    do Execute 0 program
    printfn "%i" program.[0]
    0
