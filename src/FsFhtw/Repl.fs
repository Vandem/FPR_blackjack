module Repl

open System
open Parser

type Message =
    | DomainMessage of Domain.Action
    | TableFlipRequest
    | HelpRequested
    | NotParsable of string

type State = Domain.Game

let read (input : string) =
    match input with
    | Hit -> Domain.Hit |> DomainMessage
    | Stand -> Domain.Stand |> DomainMessage
    | TableFlip -> TableFlipRequest
    | Help -> HelpRequested
    | ParseFailed  -> NotParsable input

open Microsoft.FSharp.Reflection

let createHelpText () : string =
    FSharpType.GetUnionCases typeof<Domain.Action>
    |> Array.map (fun case -> case.Name)
    |> Array.fold (fun prev curr -> prev + " " + curr) ""
    |> (fun s -> s.Trim() |> sprintf "Known commands are: %s")

let flipTable () : string =
    "(╯°□°）╯︵ ┻━┻"

let evaluate (update : Domain.Action -> State -> State) (state : State) (msg : Message) =
    match msg with
    | DomainMessage msg ->
        let newState = update msg state
        let player = newState.Players[0]
        let dealer = newState.Dealer
        let message = sprintf "The message was %A.\nYour Hand %A. Dealer's Hand %A\nYou: %A Dealer: %A" msg player.Hand dealer.Hand player.Status dealer.Status
        (newState, message)
    | TableFlipRequest ->
        let message = flipTable ()
        let newGameState = {state with State = Domain.GameOver( Domain.Lost (Domain.Busted -1)) }
        (newGameState, message)
    | HelpRequested ->
        let message = createHelpText ()
        (state, message)
    | NotParsable originalInput ->
        let message =
            sprintf """"%s" was not parsable. %s"""  originalInput "You can get information about known commands by typing \"Help\""
        (state, message)

let print (state : State, outputToPrint : string) =
    printfn "%s\n" outputToPrint
    printf "> "

    state

let rec loop (state : State) =
    match state.State with
    | Domain.GameOver x -> printfn "Game Over: %A" x
    | _ ->
    Console.ReadLine()
    |> read
    |> evaluate Domain.update state
    |> print
    |> loop
