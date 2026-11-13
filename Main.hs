
type Fingers = Int

newtype HandIndex = HandIndex Int
    deriving (Show, Eq)

data PlayerId
    = PlayerOne
    | PlayerTwo
    deriving (Show, Eq)

data Hand = Hand
    { fingers    :: Fingers
    , maxFingers :: Fingers
    } deriving (Show, Eq)

data Player = Player
    { playerId :: PlayerId
    , hands    :: [Hand]
    } deriving (Show, Eq)

data Game = Game
    { players       :: [Player]
    , currentPlayer :: PlayerId
    } deriving (Show, Eq)

data Move
    = Hit HandIndex HandIndex
    | Split HandIndex HandIndex
    deriving (Show, Eq)

data Winner = Winner PlayerId
    deriving (Show, Eq)


checkWinner :: Game -> Maybe Winner
checkWinner game =
  case players game of
    [first, second]
      | hasLiveHand first && allHandsDead second ->
          Just (Winner (playerId first))
      | hasLiveHand second && allHandsDead first ->
          Just (Winner (playerId second))
    _ -> Nothing
  where
    hasLiveHand player = any ((> 0) . fingers) (hands player)

    allHandsDead player =
      case hands player of
        [] -> False
        playerHands -> all ((== 0) . fingers) playerHands
