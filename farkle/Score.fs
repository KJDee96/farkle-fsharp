namespace farkle

module Score =
    open farkle.Die
    open farkle.State
    type Score =
        | Zero = 0
        | SingleOne = 100
        | SingleFive = 50
        | ThreeOnes = 100
        | ThreeTwos = 200
        | ThreeThrees = 300
        | ThreeFours = 400
        | ThreeFives = 500
        | ThreeSixes = 600
    let getDiceCount (diceList: int List) = List.countBy int diceList

//    let getScore (diceList: int List) = List.sumBy (fun (key, count) -> getScorePerDice key count) (getDiceCount diceList)
    let getScorePerDice diceSide count =
        match count with
        | count when count = 3 ->
            match diceSide with
            | 1 -> Score.ThreeOnes |> int, 3
            | 2 -> Score.ThreeTwos |> int, 3
            | 3 -> Score.ThreeThrees |> int, 3
            | 4 -> Score.ThreeFours |> int, 3
            | 5 -> Score.ThreeFives |> int, 3
            | 6 -> Score.ThreeSixes |> int, 3
            | _ -> Score.Zero |> int, 0
        | count when count = 6 ->
            match diceSide with
            | 1 -> Score.ThreeOnes |> int, 6
            | 2 -> Score.ThreeTwos |> int, 6
            | 3 -> Score.ThreeThrees |> int, 6
            | 4 -> Score.ThreeFours |> int, 6
            | 5 -> Score.ThreeFives |> int, 6
            | 6 -> Score.ThreeSixes |> int, 6
            | _ -> Score.Zero |> int, 0
        | _ ->
            match diceSide with
            | 1 -> count * (Score.SingleOne |> int), count
            | 5 -> count * (Score.SingleFive |> int), count
            | _ -> Score.Zero |> int, 0
                    
    // getScores -> tally, hotDice <bool>
    let rec getScores (amountPerDice: (int*int) List) (i: int) (tally: int) (diceScored: int) (length: int) (state: State) =
        match i with
        | i when i = length ->
                if diceScored = state.amountToRoll then tally, true
                else tally, false
        | _ ->
              let key, count = amountPerDice.[i]
              let score, diceCount = getScorePerDice key count
              match score with
              | 0 ->
                getScores amountPerDice (i + 1) (tally + score) diceScored length state
              | _ ->
                getScores amountPerDice (i + 1) (tally + score) (diceScored + diceCount) length state
        