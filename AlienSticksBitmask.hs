module AlienSticksBitmask where

import AlienSticks
import Data.Bits

data CompressedGame = CompressedGame Int
    deriving (Show, Eq, Ord)

-- (Kieran) Inspired by the bitmasking for my thesis as makes it efficient for representing game. Probably isn't needed here but whaterver...

-- The bitmask layout:
-- bits 0-2 are  Player 1 left hand (3 bits)
-- bits 3-5 are  Player 1 right hand (3 bits)
-- bits 6-8 are  Player 2 left hand (3 bits)
-- bits 9-11 are Player 2 right hand (3 bits)
-- bit 12 is Current player (0 = PlayerOne, 1 = PlayerTwo)
-- bits 13+ are Turn counter

-- Turns 2 players and their hands into a single bitmask
-- Returns Nothing if the game is invalid or there isn't 2 hands per player.
encodeGame :: Game -> Maybe CompressedGame
encodeGame game
    | not (validGame game) = Nothing
    | otherwise =
        case players game of
            [first, second] ->
                case (hands first, hands second) of
                    ([p1Left, p1Right], [p2Left, p2Right]) ->
                        let turnBit = if currentPlayer game == PlayerOne then 0 else 1
                            bits = (turnCounter game `shiftL` 13)
                                .|. (turnBit `shiftL` 12)
                                .|. (fingers p2Right `shiftL` 9)
                                .|. (fingers p2Left `shiftL` 6)
                                .|. (fingers p1Right `shiftL` 3)
                                .|. fingers p1Left
                        in Just (CompressedGame bits)
                    _ -> Nothing
            _ -> Nothing

-- Unpacks a bitmask into a game
-- Uses 7 to mask out each 3 bit hand
-- also assumes maxFingers is 5
decodeGame :: CompressedGame -> Game
decodeGame (CompressedGame bits) =
    let turns = bits `shiftR` 13
        turn = if ((bits `shiftR` 12) .&. 1) == 0 then PlayerOne else PlayerTwo
        p1Left = Hand (bits .&. 7) 5
        p1Right = Hand ((bits `shiftR` 3) .&. 7) 5
        p2Left = Hand ((bits `shiftR` 6) .&. 7) 5
        p2Right = Hand ((bits `shiftR` 9) .&. 7) 5
        first = Player PlayerOne [p1Left, p1Right]
        second = Player PlayerTwo [p2Left, p2Right]
    in Game [first, second] turn turns

-- Helper to build test for games (turncounter defualts to 0)
makeTestGame :: PlayerId -> Int -> Int -> Int -> Int -> Game
makeTestGame turn p1L p1R p2L p2R =
    let turnBit = if turn == PlayerOne then 0 else 1
        bits = (turnBit `shiftL` 12)
            .|. (p2R `shiftL` 9)
            .|. (p2L `shiftL` 6)
            .|. (p1R `shiftL` 3)
            .|. p1L
    in decodeGame (CompressedGame bits)

-- Story 13: reads a bitmask as a String and converts into a Game
readGame :: String -> Game
readGame str = decodeGame (CompressedGame (read str))

-- Story 14: converts a Game into a bitmask string representation
showGame :: Game -> String
showGame game =
    case encodeGame game of
        Just (CompressedGame bits) -> show bits
        Nothing -> error "invalid game"