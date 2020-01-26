open System

// pattern matching generated errors..?
// match x with i -- i is "not defined"
// i > num -- "undexpected symbol" >
// num is number to guess
// unlimited tries - until recursive calls flood the stack

let guess num = 
    let rec ask n = 
        let input = Console.ReadLine()
        let n = Int32.Parse input
        if n = num then
            printfn "guessed"
        else
            if n > num then
                printfn "too large"
            else printfn "too small"
            ask n  
    ask

// 17
// reverse all sublists in a list of lists
// expected to write more stuff in the List.rev... 
// apparently just that is enough to pass the reverse function ..?

let rlists l =
    l |> List.map (List.rev)
// and it works


// 18
// interleave lists - yay
// compiler says 'incomplete pattern matches' 
// as in "some patterns are not covered". 
// somehow this [_] doesn't cover a list of any size w. any elements
// but this _ does, given it was matched to another list before..?
// 'fucntion' didn't work but 'match with' did...
let rec inter (xs, ys) = 
    match (xs, ys) with
    | ([], []) -> [] //already determined here that it's some kind of list
    | ([], _) -> ys
    | (_, []) -> xs
    | (_, _) -> (xs.Head :: [ys.Head]) @ inter(xs.Tail, ys.Tail) 
    // so here could match with anything
    // cause f# already knows it's a list from before
// and it works

// 19
// cut in half
// i don't understand the point of an additional "general" function
// this seems to cut fine, without the n parameter too... 
let cut lst =
    match lst with
    | [] -> ([], [])
    | _ -> (lst.[0..lst.Length/2-1], lst.[(lst.Length/2)..(lst.Length-1)])
// and it works

// 20
// shuffle
// it's just "interleave" 2.0
// for some reason doesn't determine type unless "match" or "function" used
// just calling a list's function on x not enough to tell f#
// that x is a list?
let shuffle xs:int list = 
    let rec sf (x, n) =
        match (x, n) with
        | [], 0 -> [] // determined here that it's a list
        | _, y -> 
            if y >= x.Length then [] //can't check out-of-bounds w/o an "if"
            else (x.[n - x.Length/2] :: x.[n] :: sf(x, n+1))
    sf (xs, (xs.Length/2))
// and it works

// 21 - no idea how to do it, I give up

// 22
// cartesian product of two sets
// tested with
// let la = [1; 2; 3]   let lb = ["n"; "i"; "c"; "o"];;
// got this--   val it : (int * string) list = [(1, "n"); (2, "n"); (3, "n"); (1, "i"); (2, "i");  
//              (3, "i"); (1, "c"); (2, "c"); (3, "c"); (1, "o"); (2, "o"); (3, "o")]
// every recursive call, map each elem in xs to the-
// - head of remaining ys, until ys becomes []
// type says it isn't curried... but could it be "implicitly curried"..?
// does 'e' in 'fun' refer to each element in xs that's mapped to 'fun'? it acts like that's the case
let rec cart1 = function
    | [], [] -> []
    | [], _ -> []
    | _, [] -> []
    | xs, y::ys -> List.map(fun e -> (e, y)) xs @ cart1 (xs, ys)
// works


// 23
// power set - not my solution
let rec pset = function
    | [] -> [[]]
    | (x::xs) ->
        let ys = pset xs
        List.map (fun xs' -> x::xs') ys @ ys

// 24
// transpose matrix - not my solution
let rec transp = function
    | (_::_)::_ as m ->
        List.map List.head m::transp(List.map (List.tail) m)
    | _ -> []




[<EntryPoint>]
let main argv =
    0 // return an integer exit code