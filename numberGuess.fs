open System

// pattern matching generated errors
// match x with i -- i is "not defined"
// i > 5 -- "undexpected symbol" >
// "where" doesn't help I'm done with pattern matching
// 12 is arbitrarily chosen as the number to guess

let guess num = 
    let rec ask n = 
        if n = 12 then
            printfn "guessed"
        else
            if n > 12 then
                printfn "too large"
            else printfn "too small"
            let input = Console.ReadLine()
            ask(Int32.Parse input) //now it says it has to have parentheses
    ask num

[<EntryPoint>]
let main argv =
    0 // return an integer exit code