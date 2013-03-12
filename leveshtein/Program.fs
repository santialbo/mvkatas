
module leveshtein.Main
 
open System
open System.Collections.Generic 
 
let chars = [|'a'..'z'|]
 
let Levenshtein (a: array<char>) (b: array<char>) =
    let cache = Dictionary<_, _>()
    let rec lev i j =
        if cache.ContainsKey(i, j) then cache.[i, j]
        else
            let x =
                match i, j with
                | 0, j -> j
                | i, 0 -> i
                | _, _ -> min (min (lev (i - 1) j + 1) (lev i (j - 1) + 1)) (lev (i - 1) (j - 1) + (if a.[i - 1] = b.[j - 1] then 0 else 1))
            cache.Add((i, j), x)
            x
    in lev a.Length b.Length
 
let add i c a = Seq.concat [Seq.take i a; Seq.singleton c; Seq.skip i a] |> Seq.toArray
    
let delete i a = Seq.concat [Seq.take i a; Seq.skip (i + 1) a] |> Seq.toArray
    
let change i c a = Seq.concat [Seq.take i a; Seq.singleton c; Seq.skip (i + 1) a] |> Seq.toArray
 
type Tree<'a> = Nil | Tree of 'a * list<Tree<'a>>

/// Creates a tree with all the Levenshtein minimum paths from a to b
let EditChain a b =
    let rec generate a =
        let d = Levenshtein a b
        let gens = seq {
            for i in 0..(a.Length - 1) do
                let c1 = delete i a
                if (Levenshtein c1 b) < d then yield c1 else ()
                for c in chars do
                    let c2 = change i c a
                    if (Levenshtein c2 b) < d then yield c2 else ()
            for i in 0..(a.Length) do
                for c in chars do
                    let c3 = add i c a
                    if (Levenshtein c3 b) < d then yield c3 else ()
        }
        gens
        |> Seq.map (fun g -> Tree(g, generate g))
        |> Seq.toList    
            
    in Tree(a, (generate a))

/// Flattens a Tree giving a list of all the paths from the root to the leaves
let rec Flatten = function
    | Nil -> [[]]
    | Tree(a, []) -> [[a]]
    | Tree(a, ts) ->
        let ts2 = ts |> List.map (fun t -> Flatten t) |> List.concat
        in ts2 |> List.map (fun t -> a::t)

/// Prints the edit chain paths given by Flatten
let PrettyPrint (t: char array list list) =
    let pca (a: char array) = new System.String(a)
    t |> List.iter (fun cw ->
        printf "%s" (pca (List.head cw))
        (List.tail cw) |> List.iter (fun w -> printf " -> %s" (pca w))
        printfn "")
                    
[<EntryPoint>]
let main args =
    let a = Console.ReadLine().ToCharArray() 
    let b = Console.ReadLine().ToCharArray()
    PrettyPrint (Flatten (EditChain a b))   
    0
 