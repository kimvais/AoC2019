module AoC2019.IntCode

open AoC2019.Utils

type Computer = { OpPtr: int; Mem: array<int32>; Halted: bool }

type Op =
    | Add = 1
    | Mul = 2
    | Halt = 99

let readByRef offset c = c.Mem.[c.Mem.[c.OpPtr + offset]]

let read2ByRef c = readByRef 1 c, readByRef 2 c

let binOp f c =
    let p1, p2 = read2ByRef c
    let res = f p1 p2
    let writeAddress = c.Mem.[c.OpPtr + 3]
    // printfn $"%d{p1},%d{p2} = %d{res} -> %d{writeAddress}"
    c.Mem.[writeAddress] <- res
    { c with OpPtr = c.OpPtr + 4 }

let getOp c = enum<Op> (c.Mem.[c.OpPtr])

let doOp c =
    let op = getOp c
    // printfn $"Op: %A{op}"

    match op with
    | Op.Halt -> { c with Halted = true }
    | Op.Add -> binOp (+) c
    | Op.Mul -> binOp (*) c

let bootUp fn = { OpPtr = 0; Mem = readIntCode fn; Halted = false }

let rec run c =
    match c.Halted with
    | true -> c.Mem.[0]
    | false -> run (doOp c)
