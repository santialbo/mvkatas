
module cuboides_ascii.Main

open System

let cuboide a b c =
    let rep = String.replicate

    let top =
        [0 .. c]
        |> List.rev
        |> List.map (fun i -> rep i " " + "/" + rep a "____/")
        |> fun x -> (x.Head.Replace("/", " ")::x.Tail)
        |> List.mapi (fun i x -> x + rep (min i b) "|")
        |> List.mapi (fun i x -> if i > b then x + "/" else x)

    let front = 
        [1 .. b]
        |> List.map (fun x -> "|" + rep a "____|")
        |> List.mapi (fun i x -> x + rep (min c (b - i - 1)) "|")
        |> List.mapi (fun i x -> if i > b - c - 1 then x + "/" else x)

    in top @ front |> List.iter (fun x -> printfn "%s" x)

[<EntryPoint>]
let main args = 
    cuboide 3 3 3
    cuboide 5 2 3
    cuboide 4 6 5
    Console.ReadKey |> ignore
    0