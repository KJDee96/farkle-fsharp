namespace farkle

module Turn =
    open farkle.Die
    open farkle.State
    open farkle.Score
    open farkle.Player
    open farkle.Strings

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
        state
        |> turn
