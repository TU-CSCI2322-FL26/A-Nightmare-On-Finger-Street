module AlienSticks where

-- Data Types

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

-- Valid check functions

-- Valid Hand checks the bounds of the hands
-- 1. makes sure that the hand can have at least one finger
-- 2. makes sure that the hand has a non-negative number of fingers
-- 3. makes sure that the hand has less than the maximum number of fingers


validHand :: Hand -> Bool
validHand hand =
    maxFingers hand > 0
    && fingers hand >= 0
    && fingers hand < maxFingers hand

-- Valid Player checks that the player has at least one hand and that all hands are valid
-- Uses a list comprehension to check that all hands are valid, and that the player has at least one hand. Filtering out any invalid hand
-- if the resulting list is empty then all hands are valid

validPlayer :: Player -> Bool
validPlayer player =
    not (null (hands player))
    && null [hand | hand <- hands player, not (validHand hand)]

--Checks whether the overall game state is valid
-- 1. Pattern matches first and second player. Enforces a two player game.
-- 2. Makes sure players have different player ids
-- 3. Makes sure both players are valid
-- 4. Makes sure the current player is one of the two players

validGame :: Game -> Bool
validGame game = 
    case players game of
        [first, second] ->
            playerId first /= playerId second
            && validPlayer first
            && validPlayer second
            && (currentPlayer game == playerId first || currentPlayer game == playerId second)
        _ -> False

--Fogarty story 2

-- The winner needs a live hand and the opponent needs only dead hands.

-- Checks if the game has ended and returns the winner if there is one. 
-- Returns Nothing if the game is still ongoing or invalid.
-- Uses maybe winner so invalid or ongoing games can be evaluated as Nothing
-- Without throwing an exepctions.

checkWinner :: Game -> Maybe Winner
checkWinner game
    | not (validGame game) = Nothing
    | otherwise =
        case players game of
            [first, second]
                --First Player wins if they have at least one live hand
                -- and all of the second players hands are dead
                | hasLiveHand first && allHandsDead second ->
                    Just (Winner (playerId first))
                -- Second Player wins with same conditions as first player
                | hasLiveHand second && allHandsDead first ->
                    Just (Winner (playerId second))
            -- covers games still in progress.
            _ -> Nothing
    where
        -- Is true if player has at least one hand with more than 0 fingers
        hasLiveHand player = not (null (filter ((> 0) . fingers) (hands player)))

        -- True if the player has at least one hand and all of the hands are dead
        allHandsDead player =
            case hands player of
                [] -> False
                playerHands -> all ((== 0) . fingers) playerHands




-- FOgarty issue 3

-- Moves

-- attempts to make a move in the game. It will either return an error (left)
-- or the new game state (right). 

-- Apply the move for the current player, then pass the turn.


makeMove :: Game -> Move -> Either String Game
makeMove game move
    | not (validGame game) = Left "invalid game"
    | checkWinner game /= Nothing = Left "Game already has a winner"
    | otherwise =
        -- Checks against all the legal moves for the current player. If the move is not in the list of 
        -- legal moves it returns an error. Otherwise it applies the move to the game state and returns 
        -- a new game state.
        case legalMoves game of
            Nothing -> Left "invalid game"
            Just moves
                | not (move `elem` moves) -> Left "illegal move"
                | otherwise ->
                    -- Helps identify whose turn it is
                    case players game of
                        [first, second]
                            | playerId first == currentPlayer game ->
                                Right (applyMove first second (\actor target -> [actor, target]))
                            | playerId second == currentPlayer game ->
                                Right (applyMove second first (\actor target -> [target, actor]))
                        _ -> Left "Invalid game"
  where
    -- applies valid moves and then switches the turn to the opponent.
    applyMove actor target restoreOrder =
        let (updatedActor, updatedTarget) =
                case move of

                    -- Add attack fingers to the target hand.
                    -- If the total reaches or exceeds the max fingers the targets hand dies.
                    Hit source destination ->
                        let attack = fingers (handAt source actor)
                            hit hand =
                                let total = fingers hand + attack
                                in hand { fingers =
                                    if total >= maxFingers hand then 0 else total }
                        in (actor, updateHand destination hit target)

                    -- Split the fingers between two hands of the same player.
                    Split source destination ->
                        let total = fingers (handAt source actor)
                                  + fingers (handAt destination actor)
                            half = total `div` 2
                            setFingers count hand = hand { fingers = count }
                            updated = updateHand destination (setFingers half)
                                    (updateHand source (setFingers (total - half)) actor)
                        in (updated, target) -- make opponents turn

        in game { players = restoreOrder updatedActor updatedTarget
                , currentPlayer = playerId target
                }

    handAt (HandIndex index) player = hands player !! index

    updateHand (HandIndex index) change player =
        player { hands = [if position == index then change hand else hand
                         | (position, hand) <- zip [0..] (hands player)] }


-- Story 4: Find all the legal moves

-- Returns Nothing if the game is invalid, a Just [] if game is over,
-- or a Just [Move] with all the legal moves (hit and split) for current player
legalMoves :: Game -> Maybe [Move]
legalMoves game
    | not (validGame game) = Nothing
    | checkWinner game /= Nothing = Just []
    | otherwise =
        case players game of
            [first, second]
                | currentPlayer game == playerId first ->
                    Just (movesFor first second)
                | currentPlayer game == playerId second ->
                    Just (movesFor second first)
            _ -> Nothing

-- Finds all the legal moves for the current player
movesFor :: Player -> Player -> [Move]
movesFor actor opponent = hitMoves ++ splitMoves
  where
    actorHands = zip [0..] (hands actor)
    opponentHands = zip [0..] (hands opponent)

    -- Generates all the legal attacks for current player.
    -- Legal attacks are only possible if the turn attacker and targer have
    -- each have at least one finger on their hand.
    hitMoves =
        [ Hit (HandIndex i) (HandIndex j)
        | (i, source) <- actorHands
        , (j, target) <- opponentHands
        , fingers source > 0
        , fingers target > 0
        ]

    -- Generates all the legal splits for current player.
    splitMoves =
        [ Split (HandIndex i) (HandIndex j)
        | (i, source) <- actorHands
        , (j, destination) <- actorHands
        , i /= j    -- can't split hand with self
        , let total = fingers source + fingers destination
        , let newDestination = total `div` 2    -- floor half to destinationn
        , let newSource = total - newDestination    -- ceiling half stays on source
        , newSource > 0     --Neither resulting hand can be dead after split
        , newDestination > 0
        , newSource < maxFingers source    -- Neither resulting hand can exceed max fingers 
        , newDestination < maxFingers destination
        -- Stops players from splitting if the resulting hands would be the same as before
        , newSource /= fingers source
          || newDestination /= fingers destination
        -- Precent mirror swapping (hand goes from 1 to 2 and 2 to 1)
        , newSource /= fingers destination
          || newDestination /= fingers source
        ]
        
