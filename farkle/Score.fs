namespace farkle

module Score =
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

    let getScore diceSide count =
        match count with
        | count when count >= 3 ->
            match diceSide with
            | 1 -> Score.ThreeOnes |> int
            | 2 -> Score.ThreeTwos |> int
            | 3 -> Score.ThreeThrees |> int
            | 4 -> Score.ThreeFours |> int
            | 5 -> Score.ThreeFives |> int
            | 6 -> Score.ThreeSixes |> int
            | _ -> Score.Zero |> int
        | _ ->
            match diceSide with
            | 1 -> count * (Score.SingleOne |> int)
            | 5 -> count * (Score.SingleFive |> int)
            | _ -> Score.Zero |> int

    let getDiceCount (diceList: int List) = List.countBy int diceList

    let sumDice (diceList: int List) = List.sumBy (fun (key, count) -> getScore key count) (getDiceCount diceList)