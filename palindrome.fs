open System

let palin (line:string) =
    let rec check in1 in2 = 
        in1 >= in2 || line.[in1] = line.[in2] && check (in1+1) (in2-1)
    check 0 (line.Length-1)

[<EntryPoint>]
let main argv =
    let isPlr = palin "-serve evres-"
    0 // return an integer exit code