1. New connection
   1. New client --> Server = _
   1. Server --> New client = current game state (GameInProcess = true/false, all board cards, who's turn)

1. Bet
   1. Client --> Server =  bet amount
   1. Server --> Client = 'Bet accepted' // 'Cannot bet, game in progress' // 'You already placed bet'

1. Game start
   1. Server --> All clients = current game state (GameInProcess = true, initial board cards, who's turn)

1. Client move
   1. Client --> Server = Action (one more card // enough // double down? // surrender?? // split??? // insurance????)  
   1. Server --> All clients = current game state (GameInProcess = true, all board cards, who's turn) // 'Not your turn'

1. Game evaluation (Might be triggered from Client move, when last player ends turn)
   1. Server --> All clients = current game state (GameInProcess = false, all board cards (with dealer cards))
   1. Server --> Every client individually = Amount won ( 0 if lost )
   


Game state will most probably look like:

    GameInProcess: boolean
    PlayerStates: List[PlayerState]
    YourState: YourState

Where    

    PlayerState(isInGame: boolean, cards: List[Card], CurrentScore: Int, turnNow: boolean)
    YourState(state: PlayerState, bet: BigDecimal, possibleActions: List[Action])
    

