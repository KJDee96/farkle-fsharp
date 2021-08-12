namespace farkle

module Player =
    type Player =
        {name: string
         score :int
         turn: bool}
        static member Default =
            { name = ""
              score = 0
              turn = false }

    
    let updatePlayerName (name: string) (player: Player) = {player with name = name}
    
    let updatePlayerScore (score: int) (player: Player) = {player with score = score}
    
    let updatePlayerTurn (turn: bool) (player: Player) = {player with turn = turn}
    
    