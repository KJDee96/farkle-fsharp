namespace farkle

open System

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
        | Some _ ->
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
    
    let rec addPlayers (amount: int) (tally: int) (state: State) =
        match amount with
        | amount when amount = tally ->
            state
        |_ ->
            printfn "%s" enterPlayerNameString
            let name = Console.ReadLine ()
            updatePlayerName name Player.Default
            |> addPlayer <| state
            |> addPlayers amount (tally + 1)
            
    let rec getPlayers (state: State) =
        printfn "%s" amountOfPlayersString
        let _, input = getInput ()
        match input with
        | input when input >= 2 ->
            addPlayers input 0 state
        | _ ->
            printfn "%s" errorPlayerAmountString
            getPlayers state
        
    let rec turn (state: State) =
        let currentPlayer = getCurrentPlayer state

        let dice = rollSet state.dice state.amountToRoll
        let diceCount = getDiceCount dice
        printfn "%A" dice
        let tally,hotdice = getScores diceCount 0 0 0 diceCount.Length state
        
        match hotdice with
        | true ->
            printfn "Score: %A HotDice: %A" tally hotdice
            
            updatePlayerScore tally state.players.[currentPlayer]
            |> updatePlayer currentPlayer <| state
            |> turn
            
        | false -> 
            printfn "Score: %A HotDice: %A" tally hotdice
            
            let newState = updatePlayerScore tally state.players.[currentPlayer]
                           |> updatePlayerTurn false
                           |> updatePlayer currentPlayer <| state
            
            
            let player = List.tryItem (currentPlayer + 1) newState.players
            match player with
            | Some _ ->
                updatePlayerTurn true newState.players.[currentPlayer + 1]
                |> updatePlayer (currentPlayer + 1) <| newState
                |> turn
            | None ->
                updatePlayerTurn true newState.players.[0]
                |> updatePlayer 0 <| newState
                |> turn
                
    let playFarkle (state: State) : State =
        let stateWithPlayer = state |> getPlayers
        
        updatePlayerTurn true stateWithPlayer.players.[0]
        |> updatePlayer 0 <| stateWithPlayer
        |> turn

    let changeSettings (state: State) : State =
        settingsMenu state
    
    let rec mainMenu state =
        printfn "%s" menuString
        match getInput () with
        | _, 1 -> playFarkle state |> mainMenu
        | _, 2 -> changeSettings state |> mainMenu 
        | _, 3 -> Environment.Exit 0
        | _ ->
            printfn "%s" errorInvalidInputString
            mainMenu state
   
    [<EntryPoint>]
    let main argv =
        let initState = State.Default
//        printfn "%A" initState

//        printfn "%A" dice
//        printfn "%A" diceCount
//        printfn "%i" score
//        
//        let allDice = getAllDice
//        let updatedState = updateDice initState allDice.[1]
//        
//        printDiceChoice allDice
//        printfn "%A" updatedState
        
//        updatePlayerTurn true initState.players.[0]
//        |> updatePlayer 0 <| initState
//        |> getCurrentPlayer
//        |> printfn "%A"
        printfn $"%s{titleString}"
        mainMenu State.Default
        
        0 // return an integer exit code