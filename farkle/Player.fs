namespace farkle

module Player =
    type Player =
        {name: string
         score :int}
        static member Default =
            { name = ""
              score = 0 }

    
    let updatePlayerName (name: string) (player: Player) = {player with name = name}
    
    let updatePlayerScore (score: int) (player: Player) = {player with score = score}