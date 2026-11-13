
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


--Fogarty story 2



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




-- FOgarty issue 3


makeMove :: Game -> Move -> Game
makeMove game move =
    case players game of
        [first, second]
            | playerId first == currentPlayer game ->
                applyMove first second (\actor target -> [actor, target])
            | playerId second == currentPlayer game ->
                applyMove second first (\actor target -> [target, actor])
        _ -> game
  where
    applyMove actor target restoreOrder =
        let (updatedActor, updatedTarget) =
                case move of
                    Hit source destination ->
                        let attack = fingers (handAt source actor)
                            hit hand =
                                let total = fingers hand + attack
                                in hand { fingers =
                                    if total >= maxFingers hand then 0 else total }
                        in (actor, updateHand destination hit target)

                    Split source destination ->
                        let total = fingers (handAt source actor)
                                  + fingers (handAt destination actor)
                            half = total `div` 2
                            setFingers count hand = hand { fingers = count }
                            updated = updateHand destination (setFingers half)
                                    (updateHand source (setFingers (total - half)) actor)
                        in (updated, target)

        in game { players = restoreOrder updatedActor updatedTarget
                , currentPlayer = playerId target
                }

    handAt (HandIndex index) player = hands player !! index

    updateHand (HandIndex index) change player =
        player { hands = [if position == index then change hand else hand
                         | (position, hand) <- zip [0..] (hands player)] }