module AoC2019.Day2

open AoC2019.Utils
open AoC2019.IntCode

let day2 fn () =
    let c = bootUp fn
    c.Mem.[1] <- 12
    c.Mem.[2] <- 2
    run c |> int64

let day2part2 fn () =
    let initialMemory = readIntCode fn
    let verbNouns = Seq.allPairs [0..99] [0..99]
    verbNouns |> Seq.find (fun (n,v) ->
        let c = reboot initialMemory ()
        c.Mem.[1] <- n
        c.Mem.[2] <- v
        match run c with
        | 19690720 -> true
        | _ -> false
    ) |> fun (n,v) -> n*100+v|> int64