open System

[<EntryPoint>]
let main argv =
    printfn "Welcome to the Blackjack!"
    printfn "Please enter your commands to interact with the system."
    printfn "Press CTRL+C to stop the program."
    printfn "Enter your Name to start"
    printf "> "

    let playerName = Console.ReadLine()
    let initialState = Domain.init playerName
    Repl.evaluate Domain.update initialState (Repl.Message.DomainMessage Domain.Info)
    |> Repl.print
    Repl.loop initialState
    0 // return an integer exit code
