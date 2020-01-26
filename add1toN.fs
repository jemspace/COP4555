open System

let rec addnum n = 
    match n with
        | 1 -> 1
        | n -> n + addnum(n-1)

[<EntryPoint>]
let main argv =
    let number = int(Console.ReadLine())
    let sum = addnum number
    0 // return an integer exit code