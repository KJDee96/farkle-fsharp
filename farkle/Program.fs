namespace farkle

module Program =
    open farkle.State
    open farkle.Strings
    open farkle.MainMenu
    
    [<EntryPoint>]
    let main argv =
        printfn $"%s{titleString}"
        mainMenu State.Default
        
        0 // return an integer exit code