module AoC2019.IntCode
open AoC2019.Utils

type Computer = { PTR: int64; memory: Map<int64, int64>; stopped: bool }

type Operation = | Add | Mul | Halt

let matchOpCode opcode =
    match opcode with
    | 1L -> Add
    | 2L -> Mul
    | 99L -> Halt
    | _ -> failwith "Unknown op code %d"

let doOp c opcode p1 p2 =
    match opcode with
    | Add -> p1 + p2
    | Mul -> p1 * p2
    | Halt -> failwith (sprintf "%d" (c.memory |> Map.find 0L))

let bootUp code =
    let state = code |> Seq.mapi (fun i x -> (int64 i, x)) |> Map.ofSeq
    { PTR = 0L; memory = state; stopped = false }

let load m addr =
    m |> Map.find addr

let getValByAddr mem addr =
    mem |> Map.find addr

let getVal state offset =
    let load' = load state.memory
    let addr = (state.PTR + offset)
    let addr' = load' addr
    let ret = load' addr'
    printfn "read value %d @ %d" ret addr'
    ret

let rec run state =
    let memory = state.memory
    let doOp' =
        doOp state
    let opcode = matchOpCode (memory |> Map.find state.PTR)
    let stopped = match opcode with
        | Halt -> true
        | _ -> false
    let p1 = getVal state 1L
    let p2 = getVal state 2L
    let addr = getVal state 3L // This is wrong, read by address, not by reference!
    let result = doOp' opcode p1 p2
    printfn "%A %d %d = %d -> %d" opcode p1 p2 result addr
    let memory = memory.Add(addr, result)
    match stopped with
        | false -> run { PTR = state.PTR + 4L; memory = memory; stopped = false }
        | true -> memory |> printfn "%A"
    
        


