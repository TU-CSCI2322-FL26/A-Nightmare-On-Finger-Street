module AlienSticksBitmask where

import AlienSticks
import Data.Bits

data CompressedGame = CompressedGame Int
    deriving (Show, Eq, Ord)

encodeGame :: Game -> Maybe CompressedGame
encodeGame game
    | not (validGame game) = Nothing
    | otherwise =
        case players game of
            [first, second] ->
                case (hands first, hands second) of
                    ([p1Left, p1Right], [p2Left, p2Right]) ->
                        let turnBit = if currentPlayer game == PlayerOne then 0 else 1
                            bits = (turnBit `shiftL` 12)
                                .|. (fingers p2Right `shiftL` 9)
                                .|. (fingers p2Left `shiftL` 6)
                                .|. (fingers p1Right `shiftL` 3)
                                .|. fingers p1Left
                        in Just (CompressedGame bits)
                    _ -> Nothing
            _ -> Nothing

decodeGame :: CompressedGame -> Game
decodeGame (CompressedGame bits) =
    let turn = if ((bits `shiftR` 12) .&. 1) == 0 then PlayerOne else PlayerTwo
        p1Left = Hand (bits .&. 7) 5
        p1Right = Hand ((bits `shiftR` 3) .&. 7) 5
        p2Left = Hand ((bits `shiftR` 6) .&. 7) 5
        p2Right = Hand ((bits `shiftR` 9) .&. 7) 5
        first = Player PlayerOne [p1Left, p1Right]
        second = Player PlayerTwo [p2Left, p2Right]
    in Game [first, second] turn

makeTestGame :: PlayerId -> Int -> Int -> Int -> Int -> Game
makeTestGame turn p1L p1R p2L p2R =
    let turnBit = if turn == PlayerOne then 0 else 1
        bits = (turnBit `shiftL` 12)
            .|. (p2R `shiftL` 9)
            .|. (p2L `shiftL` 6)
            .|. (p1R `shiftL` 3)
            .|. p1L
    in decodeGame (CompressedGame bits)