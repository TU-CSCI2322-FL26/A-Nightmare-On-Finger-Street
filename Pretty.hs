module Pretty where
import AlienSticks

-- Story 5
-- Formating

-- Makes the full game human readable
showGame :: Game -> String
showGame game
    | not (validGame game) = "Invalid game"
    | otherwise =
        unlines (map showPlayer (players game)) ++
        "Current player: " ++ showPlayerId (currentPlayer game) ++ "\n"

-- makes the player id human readable
showPlayerId :: PlayerId -> String
showPlayerId PlayerOne = "Player One"
showPlayerId PlayerTwo = "Player Two"

-- formats the player name and their hands human readable
showPlayer :: Player -> String
showPlayer player = 
    showPlayerId (playerId player) ++ ": " ++ unwords (map showHand (hands player))

-- Format hand to current fingers/Max Fingers
showHand :: Hand -> String
showHand hand = "[" ++ show (fingers hand) ++ "/" ++ show (maxFingers hand) ++ "]"
