module AoC2019.Day1

open AoC2019.Utils

let calcFuel wt = int64 wt / 3L - 2L

let fuelMass wt =
    wt
    |> Seq.unfold
        (function
        | n when n > 0L -> Some(n, n / 3L - 2L)
        | _ -> None)

let calcFuelRequirement wt = fuelMass wt |> Seq.sum |> (+) -wt

let calcFuelMass s = s |> int64 |> calcFuelRequirement

let day1 fn () = readInput fn |> Seq.sumBy calcFuel

let day1part2 fn () = readInput fn |> Seq.sumBy calcFuelMass
