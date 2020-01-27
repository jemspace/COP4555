open System
// ===================== INTRO PROBLEMS ======================
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

// ===================== PROBLEM SET 1 ======================
// 17
// reverse all sublists in a list of lists
// expected to write more stuff in the List.rev... 
// apparently just that is enough to pass the reverse function ..?
let rlists l =
    l |> List.map (List.rev)


// 18
// interleave lists
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

// 19
// cut in half
// i don't understand the point of an additional "general" function
// this seems to cut fine, without the n parameter too... 
let cut lst =
    match lst with
    | [] -> ([], [])
    | _ -> (lst.[0..lst.Length/2-1], lst.[(lst.Length/2)..(lst.Length-1)])

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


// 23
// for each element, a subset of powerset either includes it or not
// for list of n elements yields a powerset of 2^n
// each recursive call is either with an element or without
// (full list or just tail)
// --- version 1
let rec powerset = function
    | [] -> [[]]
    | (x::xs) -> List.map(fun xs -> x::xs) xs @ (powerset xs) // errors (-_-")

// --- version 2
// not a full power set
let rec powerset = function
    | [] -> [[]]
    | (x::xs) -> 
        let xs2 = powerset xs
        List.map(fun xs -> x::xs) [xs] @ xs2


// 24


// 25
// has all 3 rules for recursion
let rec sort = function
    | [] -> [] //base case covered
    | [x] -> [x] //base case covered
    | x1::x2::xs ->
        if x1 <= x2 then (x1 :: sort (x2::xs)) //sorting progressively smaller sublists (xs)
        else (x2 :: sort (x1::xs))

// 26
// supposedly the last thing should be a recursive call,
// but in one of these it isn't the last
let rec merge = function
    | ([], ys) -> ys
    | (xs, []) -> xs
    | (x::xs, y::ys) -> 
        if x<y then x::merge(xs,y::ys)
        else y::merge(x::xs, ys)

// the split works not by splitting the list in half
// but by putting every other element in one list, every next in the other?
let rec split = function
    | [] -> ([], []) 
    | [a] -> ([a], [])
    | a::b::cs -> 
        let (M, N) = split cs
        (a::M, b::N) //not recursive call? a "let" value binding

let rec mergesort = function
| [] -> []
| L ->
    let (M, N) = split L
    merge (mergesort M, mergesort N) //last thing is recursive call as should be
    


[<EntryPoint>]
let main argv =
    0 // return an integer exit code
