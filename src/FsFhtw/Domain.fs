module Domain

type Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace

type Suit = Hearts | Clubs | Spades | Diamonds

type Points = Single of int | Double of int*int

type Card = Suit * Rank

type Hand = Card list

type Deck = Card list

type ShuffledDeck = Card list

type Score = int

type Status = Blackjack | Busted of Score | Stood of Score | CardsDealt of Score

type Player = { Hand: Hand; Status: Status; Name: string; Id: int }

type Dealer = { Hand: Hand; Status: Status }

type RoundResult = Won of Status | Lost of Status | Draw of Status

type GameState = Init | Playing | GameOver of RoundResult

type Game = { Deck: Deck; Dealer: Dealer; Players: Player list; State: GameState }

type Action = Hit | Stand | Info

let getRank (card:Card) =
    snd card

let getValue (card: Card): Points =
    match getRank card with
    | Ace -> Double(1,11)
    | Jack | Queen | King | Ten -> Single(10)
    | Two -> Single(2)
    | Three -> Single(3)
    | Four -> Single(4)
    | Five -> Single(5)
    | Six -> Single(6)
    | Seven -> Single(7)
    | Eight -> Single(8)
    | Nine -> Single(9)

let isDouble (point: Points): bool =
    match point with
    | Double(x, y) -> true
    | Single(x) -> false

let getMin (point: Points): int =
    match point with
    | Double(x, y) -> x
    | Single(x) -> x

let getMax (point: Points): int =
    match point with
    | Double(x, y) -> y
    | Single(x) -> x

let isBusted (score: Score): bool =
    match score with
    | 21 -> false
    | p when p > 21 -> true
    | p -> false

let getPlayerState (score: Score) (action: Action): Status =
    match score, action with
    | 21,_ -> Blackjack
    | p,_ when p > 21 -> Busted p
    | p,Hit -> CardsDealt p
    | p,Stand -> Stood p

let sumPoints handleAce (hand: Hand) : Score =
    hand
    |> List.map getValue
    |> List.map handleAce
    |> List.sum


let evaluateHand (hand: Hand) (action: Action): Status =
    let max = sumPoints getMax hand
    let min = sumPoints getMin hand
    if isBusted max then
        getPlayerState min action
    else
        getPlayerState max action

let drawCard deck =
    match deck with
    | [] -> None
    | topCard::restOfDeck -> Some (topCard, restOfDeck)

type MaybeBuilder() =
    member this.Bind(input, func) =
        match input with
        | None -> None
        | Some value -> func value

    member this.Return value =
        Some value

let setupPlayer drawCard name id (deck: ShuffledDeck) =
    let maybe = new MaybeBuilder()

    maybe {
        let! (firstCard: Card, deck) = drawCard deck
        let! (secondCard: Card, deck) = drawCard deck
        let hand: Card list = [firstCard; secondCard]
        let status = evaluateHand hand Hit

        return { Hand = hand; Status = status; Name = name; Id = id; }, deck
    }

let setupDealer drawCard (deck: ShuffledDeck) =
    let maybe = new MaybeBuilder()

    maybe {
        let! firstCard, deck = drawCard deck
        let hand = [firstCard]
        let status = evaluateHand hand Hit

        return { Hand = hand; Status = status;}, deck
    }

//open Microsoft.FSharp.Reflection
//let ranks = FSharpType.GetUnionCases typeof<Rank>
//let suits = FSharpType.GetUnionCases typeof<Suit>
let private suits = [Hearts; Clubs; Spades; Diamonds]
let private ranks = [Two; Three; Four; Five; Six; Seven; Eight; Nine; Ten;
                     Jack; Queen; King; Ace]

let getGameState (playerStatus: Status) (dealerStatus: Status): GameState =
    match playerStatus, dealerStatus with
    | Stood playerScore, Stood dealerScore ->
        if playerScore > dealerScore then GameOver (Won (Stood playerScore))
        else if playerScore = dealerScore then GameOver (Draw (Stood playerScore))
        else GameOver (Lost (Stood playerScore))
    | Stood playerScore, Busted _ -> GameOver (Won (Stood playerScore))
    | Stood playerScore, Blackjack _ -> GameOver (Lost (Stood playerScore))
    | Busted playerScore, _ -> GameOver (Lost (Busted playerScore))
    | Blackjack , Busted _ -> GameOver (Won Blackjack)
    | Blackjack , Stood _ -> GameOver (Won Blackjack)
    | Blackjack , Blackjack _ -> GameOver (Draw Blackjack)
    | _ -> Playing

let shuffle (deck: Deck): ShuffledDeck =
    let rnd = System.Random ()
    deck |> List.sortBy(fun _ -> rnd.Next(1, 52) )

let addCardToHand (hand: Hand) (card: Card) =
    card::hand

let isAt17OrHigher (status: Status) =
    match status with
    | Busted x -> true
    | Blackjack -> true
    | Stood x when x >= 17 -> true
    | Stood x when x < 17 -> false
    | CardsDealt x when x >= 17 -> true
    | CardsDealt x when x < 17 -> false

let rec drawUntil17OrBust (dealer: Dealer) (deck: Deck) (currentHand: Hand) =
    if isAt17OrHigher <| evaluateHand currentHand Hit then
        let newDealerState = evaluateHand currentHand Stand
        {Hand = currentHand; Status = newDealerState}, deck
    else
        let card,deck = Option.get (drawCard deck)
        let newHand = addCardToHand dealer.Hand card
        drawUntil17OrBust {Hand = newHand; Status = dealer.Status} deck newHand

let isDealersTurn (status: Status) =
    match status with
    | Busted x -> true
    | Stood x -> true
    | Blackjack -> true
    | CardsDealt x -> false

let hit (game: Game): Game =
    let player = game.Players[0]
    let dealer = game.Dealer
    let card,deck = Option.get (drawCard game.Deck)
    let newHand = addCardToHand player.Hand card
    let newPlayerStatus = (evaluateHand newHand Hit)
    let updatedPlayer: Player = {Hand = newHand; Status = newPlayerStatus; Name = player.Name; Id = player.Id}

    if isDealersTurn newPlayerStatus then
        let updatedDealer,deck = drawUntil17OrBust dealer deck dealer.Hand
        {Deck = deck; Dealer = updatedDealer; Players = [updatedPlayer]; State = getGameState newPlayerStatus updatedDealer.Status}
    else
        {Deck = deck; Dealer = dealer; Players = [updatedPlayer]; State = getGameState newPlayerStatus dealer.Status}

let stand (game: Game): Game =
    let player = game.Players[0]
    let newPlayerStatus = (evaluateHand player.Hand Stand)
    let updatedPlayer: Player = {player with Status = newPlayerStatus}
    let dealer = game.Dealer
    let updatedDealer,deck = drawUntil17OrBust dealer game.Deck dealer.Hand
    {Deck = deck; Dealer = updatedDealer; Players = [updatedPlayer]; State = getGameState newPlayerStatus updatedDealer.Status}

let init (playerName: string) : Game =
    let fullDeck: Deck = 
        [ 
            for s in suits do
                for r in ranks do
                     yield Card(s, r)
        ]

    let shuffledDeck = shuffle fullDeck
    let dealer,deck = Option.get (setupDealer drawCard shuffledDeck)
    let player,deck = Option.get (setupPlayer drawCard playerName 0 deck)

    {Deck = deck; Dealer = dealer; Players = [player]; State = Playing}


let update (msg : Action) (game : Game) : Game =
    match msg with
    | Hit -> hit game
    | Stand -> stand game
    | Info -> game
