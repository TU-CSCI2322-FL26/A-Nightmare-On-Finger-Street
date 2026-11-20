module Solver where
import AlienSticks

whoWillWin ::  Game -> Winner
whoWillWin  game = 
    --checks to see if someone has already won
    case checkWinner game of 
        Just w  -> w

        --maxturns should be 9(~35s) to test draws, 
        --for now i'll leave it at 500 until we do story 11
        Nothing ->
            case legalMoves game of 
                Nothing    -> Draw
                Just []    -> Draw
                Just moves -> pickBest outcomes
                    where 
                        --nextGames goes through each move and returns [Game], only keeps valid moves.
                        --outcomes is the recursion 
                        nextGames = [g | mv <- moves, Right g <- [makeMove  game mv]]
                        outcomes = map whoWillWin nextGames
    where 
        currPlayer = Winner (currentPlayer game)
        --picks best move to take for the current player, otherwise is if 
        -- all moves lead to other player winning, so it returns one of the losing outcomes. 
        pickBest outcomes
                | currPlayer `elem` outcomes = currPlayer
                | Draw `elem` outcomes       = Draw
                | null outcomes              = Draw
                | otherwise                  = head outcomes 
