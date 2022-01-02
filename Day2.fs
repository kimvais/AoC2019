module AoC2019.Day2

open AoC2019.Utils
open AoC2019.IntCode

let day2 fn () =
    let c = bootUp fn
    c.Mem.[1] <- 12
    c.Mem.[2] <- 2
    run c |> int64