namespace farkle

module State =
    open farkle.Die
    open farkle.Player
    
    type State =
        { dice: Die
          amountToRoll: int
          players : Player List }

        static member Default =
            { dice = Die.D6
              amountToRoll = 5
              players = [] }

//    let updatePlayerScore (state: State) (playerIndex: int) = { state with players =  }
    
    let updateListElement index newValue list =
        List.indexed list
        |> List.map (fun (key, value) ->
            if key = index then key, newValue else key, value)
        |> List.map snd // unindex list
    
    let addPlayer  (player: Player) (state: State) =
        let newPlayers = List.append state.players [player]
        { state with players = newPlayers }

    let updatePlayer (index: int) (player: Player) (state: State) =
        let newPlayers = updateListElement index player state.players
        { state with players = newPlayers }
        
    let updateDice (die: Die) (state: State) = { state with dice = die }
    
    let updateAmountToRoll (amount: int) (state: State) = { state with amountToRoll = amount }
    
    let printState (state: State) =
        printfn "%A" state
        state
    let getCurrentPlayer (state: State) =
        List.findIndex (fun (x:Player) -> x.turn = true) state.players
