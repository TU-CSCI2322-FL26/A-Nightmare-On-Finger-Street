-- Hello World
-- Dr. Fogarty is awesome and everyone should know.

{- 
There are two components for this sprint. In total, you will add 10 stories (pieces of functionality) to your list of tasks to be finished by the sprint, called the backlog. Many of these stories include specific function names and type signatures: this is usually bad practice, but is useful for this course. Feel free to change the names or type signatures.

Story 7: Before you create any new branches, decompose your project into modules. I suggest one module for core gameplay, one module for pretty printing/input/output, and one module for solving games. A module for test cases is also a good idea.

Estimate: 2

First, you will solve games by determining the optimal move for a player for a game state. We will add the following stories for solving the game to our backlog:

Story 8: If necessary, ensure that your game has bounded depth. For games that can cycle, such as checkers and chess, add a turn counter and tie the game when it hits zero.

Estimate: 2


Story 9: Refactor a Winner or Outcome type that represents a win or tie (but not an ongoing game).

Have your "check who has won" function, from story 2, be of type  Game -> Maybe Winner.

Estimate: 1


Story 10: Write  a function whoWillWin :: Game -> Winner. 

Considers every valid move, the resulting game state, and chooses the best outcome for the current player. This will involve recursively searching through the game states that result from that move.  Think Scavenge!

You must complete this story before starting story 11!

Estimate: 9, requires story 8.


Story 11: Write a function bestMove :: Game -> Move that takes a Game and return the best Move. 

Given a game state, you should  return a move that can force a win for the current player. Failing that, return a move that can force a tie for the current player. 

This is very similar to whoWillWin, but keeps track of what the first move was that could force that outcome. That means you should not use this function to write whoWillWin.

Estimate: 5, requires story 10.


Second, you will implement a simple interface. Your program should read a file name from standard input or an argument, solve the game, and print the winning move. This can be broken down into the following stories, which we will add to our backlog.

Story 12: Design simple text format that is easy for your program to read and write. It should probably be different  your "pretty show" from the first sprint. Then, define at least four files in that format, representing games in various states of play. 

The input format should describe the board game in progress, and can look very similar to your internal representation. 

For instance, each square is a 0 for blank, 1 for player 1, or 2 for player 2. The current turn or other intangible components are given in the first (or last) few lines.

I suggest you use newlines, spaces, or other delimiters that afford the use of lines, words, and splitOn.

You will need games that are only a few moves from the end. I suggest at least one each that is finished, one move, two moves, and four moves from the end. Make sure they can be won or lost based on different choices. 

Caveat: In this context, "from the end" means "worst case number of moves, no matter how either player plays." Even if there is a way to win in one move, you might end up searching every other move first - and taking those all the way to the end of the game.

Estimate: 2


Story 13: Define a function readGame :: String -> Game that takes a string in your text format and returns the corresponding game.

You should design your format in story 11. to make this as easy as possible.

Estimate: 2, requires story 12


Story 14: Define a function showGame :: Game -> String that takes a game and turns it into a string in your text format.

Ensure that readGame (showGame game) works for a variety of inputs!

Do not make a custom instance of the Show typeclass

The opposite of  splitOn is intercalate.

Estimate: 1, requires story 12


Story 15: Define a series of IO actions to wrap around these functions, as well as bestMove, and define a main IO action. You will need to build four I/O actions: one each to read and write game states from a file, one that computes and prints the winning move, and a simple main action. 

At least some of these actions will need to be in a Main module that imports your other module(s).

writeGame :: Game -> FilePath -> IO ()

loadGame :: FilePath -> IO Game 

putBestMove :: Game -> IO () that computes the best move and prints it to standard output. For full credit, also print the outcome that moves forces.

main :: IO (), which reads a file name from standard input or the arguments, loads the game, and prints the best move.

Estimate: 3, requires stories 11, 12, 13, and 14


There are also a couple of stories related to testing and error handling. In proper software development, these would be part of every story. For this course, we have split them into their own. Note story 6 is extended here.

Story 16: Define several test cases for each function: games to compute valid moves, who has won, who will win, and the best move from; games that should result from making moves on those games; and strings to convert to and from games. Some of these may overlap with Story 11.

Estimate: 3, lower priority.


Story 6 (Full Credit): All functions should consider possible errors or edge cases. Return a Maybe Move, Maybe Game,  etc. as appropriate. 

Estimate: 2-5, depending on game, which functions needs to change, and familiarity with Maybe and do blocks. 
-}