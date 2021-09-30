namespace farkle

module Turn =
    open System
    open farkle.Die
    open farkle.State
    open farkle.Score
    open farkle.Player
    open farkle.Strings
    
    let rec pressEnter (messageSent: bool) =
        if messageSent then () else printfn "%s" pressEnterString
        let keyStroke = Console.ReadKey(true) 
        if keyStroke.Key.Equals(ConsoleKey.Enter) then ()
        else pressEnter true
        
    let getNextPlayer (currentPlayer: int) (state: State) =
        let nextPlayer =
            List.tryItem (currentPlayer + 1) state.players

        match nextPlayer with
        | Some _ ->
            updatePlayerTurn true state.players.[currentPlayer + 1]
            |> updatePlayer (currentPlayer + 1) <| state
        | None ->
            updatePlayerTurn true state.players.[0]
            |> updatePlayer 0 <| state
    
    let rec printDiceInput (amount: int) (count: int) =
        match count with
        | count when count = amount ->
            printf "\n"
            ()
        | _ ->
            printf "[%i] " count
            printDiceInput amount (count + 1)
    
    let rec printDice (dice: int List) (amount: int) (count: int) =
        match count with
        | count when count = amount ->
            printf "\n"
            ()
        | _ ->
            printf "%i   " dice.[count]
            printDice dice amount (count + 1)
    
    let rec checkCounts (input: (int*int) []) (acc: int) (state: State) : bool =
        match acc with
        | acc when acc = input.Length -> true
        | _ ->
            let key, count = input.[acc]
            if key > (state.dice |> int) then false
            else
                match key, count with
                | _, count when count > 1 -> false
                | _ -> checkCounts input (acc + 1) state
        
    let rec getDiceChoiceInput (state: State) =
        let input = Console.ReadLine().Split ' '
        let ints = Array.map (fun (x :string) ->
                                let _,value = Int32.TryParse x
                                value) input
        match checkCounts (Array.countBy int ints) 0 state with
        | true -> ints
        | false ->
            printfn "%s" errorInvalidInputString
            getDiceChoiceInput state
        

    let changeDice (dice: int List) (state: State) =
        printfn "Which dice do you want to keep (enter each separated by a space e.g 1 2 3)"
        
        printDiceInput state.amountToRoll 0
        printDice dice state.amountToRoll 0
        let diceToChange = getDiceChoiceInput state
        let diceToKeep = Array.toList (Array.map (fun (x: int) -> List.item x dice) diceToChange) 
        List.concat [diceToKeep; (rollSet state.dice (state.amountToRoll - diceToKeep.Length))]
        
    let rec turn (hotDicePrevious: bool) (state: State) =
        let currentPlayer = getCurrentPlayer state
        match hotDicePrevious with
        | true ->
            printfn "HOT DICE It's %s's turn again!" state.players.[currentPlayer].name
        | false ->
            printfn "It's %s's turn!" state.players.[currentPlayer].name
        
        let dice = rollSet state.dice state.amountToRoll
        let newDice = changeDice dice state
        printfn "\nNew Dice:"
        printDice dice state.amountToRoll 0
        let diceCount = getDiceCount newDice

        
        let tally, hotDice =
            getScores diceCount 0 0 0 diceCount.Length state
        
        match hotDice with
        | true ->
            printfn "Score: %A HotDice: %A" tally hotDice
            pressEnter false
            updatePlayerScore tally state.players.[currentPlayer]
            |> updatePlayer currentPlayer <| state
            |> turn hotDice

        | false ->
            printfn "Score: %A HotDice: %A" tally hotDice
            pressEnter false
            updatePlayerScore tally state.players.[currentPlayer]
            |> updatePlayerTurn false
            |> updatePlayer currentPlayer <| state
            |> getNextPlayer currentPlayer
            |> turn hotDice
            

    let playFarkle (state: State) : State = state |> turn false
