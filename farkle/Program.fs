namespace farkle

open System
open System.Collections.Generic

module Program =
    open farkle.Die
    open farkle.State
    open farkle.Score
    open farkle.Player
    open farkle.Strings
    
    let getInput () = Console.ReadLine () |> Int32.TryParse
    
    let rec amountInput (state: State) =
        let _, input = getInput ()
        match input with
        | input when input >= 5 ->
            updateAmountToRoll input state
        | _ ->
            printfn "%s" errorAmountString
            amountInput state
            
    let rec diceInput (state: State) =
        let allDice = getAllDice
        printDiceChoice allDice
        
        let _, input = getInput ()
        let dice = List.tryItem (input - 1) allDice
        match dice with
        | None ->
            printfn "%s" errorInvalidInputString
            diceInput state
        | Some(_) ->
            updateDice dice.Value state
        
    let rec settingsMenu state = 
        printfn "%s" settingsMenuString
        match getInput () with
        | _, 1 ->
            printfn "%s" chooseDiceString
            diceInput state
            |> settingsMenu
        | _, 2 ->
            printfn "%s" amountToRollString
            amountInput state
            |> settingsMenu
        | _, 3 -> state
        | _ ->
            printfn "%s" errorInvalidInputString
            settingsMenu state
            
    let playFarkle (state: State) : State = state

    let changeSettings (state: State) : State =
            settingsMenu state
    
    let rec mainMenu state =
        printfn "%s" menuString
        match getInput () with
        | _, 1 -> playFarkle state |> mainMenu
        | _, 2 -> changeSettings state |> mainMenu 
        | _, 3 -> Environment.Exit 0
        | _ ->
            printfn "That was an invalid choice"
            mainMenu state
   
    [<EntryPoint>]
    let main argv =
//        let initState = State.Default
//        printfn "%A" initState
//        
//        let dice = rollSet initState.dice 5
//        let score = sumDice dice
//        
//        printfn "%A" dice
//        printfn "%i" score
//        
//        let allDice = getAllDice
//        let updatedState = updateDice initState allDice.[1]
//        
//        printDiceChoice allDice
//        printfn "%A" updatedState
        
        printfn $"%s{titleString}"
        mainMenu State.Default
        
//        Player.Default
//        |> updatePlayerName "Test"
//        |> updatePlayerScore 100
//        |> updatePlayer 0 <| State.Default
//        |> printfn "%A"

        0 // return an integer exit code