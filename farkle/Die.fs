namespace farkle

module Die =
    open System

    type Die =
        | D6 = 6
        | D12 = 12

    let roll (die: Die) = Random().Next(1, die |> int32)
    
    let rollSet (die: Die) length = List.init length (fun _ -> roll die)
    
    let getAllDice = Enum.GetValues(typeof<Die>) :?> Die [] |> Array.toList
    
    let printDiceChoice (allDice: Die List )= List.iter (fun (index, value) -> printfn "[%i] %s" (index + 1) (value.ToString())) (List.indexed allDice)