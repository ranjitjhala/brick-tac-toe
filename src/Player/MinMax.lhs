Implementation of the min-max algorithm of the tic-tac-toc game
----------------------------------------------------------------

The minmax algorithm for tic-tac-toc can be found here
<http://neverstopbuilding.com/minimax>

We start by importing useful functions and types from the `Types` and `Check` modules. 
\begin{code}
module Player.MinMax (playerMinMax) where 

import Types  (Player(..), Tile, Board, Move, validMoves, put, flipTile)
import Checks (scoreBoard)
\end{code}

Then we define the player to have the `minMax` strategy and a string name. 
\begin{code}
playerMinMax :: Player 
playerMinMax = Player minMax "MinMax"
\end{code}

The `minMax` strategy returns the move with the maximum score
as returned by `evaluateBoardMax`
\begin{code}
minMax :: Tile -> Board -> IO Move
minMax tile board 
  = return $ snd $ maximum scoredMoves 
   where
    scoredMoves = zip scores moves
    scores      = map (evaluateBoardMax tile . put board tile) moves 
    moves       = validMoves board
\end{code}

1. Define the function `evaluateBoardMax tile board` to return 

- the score of board on tile, if such score exists, otherwise
- the `minimum` score of `evaluateBoardMin` on the flipped tile, for all the valid moves of the flipped tile

\begin{code}
evaluateBoardMax :: Tile -> Board -> Int
evaluateBoardMax tile board = error "Define me!"
\end{code}

2. Dually, define the function `evaluateBoardMin tile board` to return 

- the score of board on tile, if such score exists, otherwise
- the `maximum` score of `evaluateBoardMax` on the flipped tile, for all the valid moves of the flipped tile

\begin{code}
evaluateBoardMin :: Tile -> Board -> Int
evaluateBoardMin tile board = error "Define me!"
\end{code}

That's it! Your player can never lose! 

3. Test your implementation

Replace [`player2` in `TicTacToe.hs`](https://github.com/nikivazou/tic-tac-toe/blob/master/classic/src/TicTacToe.hs#L12) with `playerMinMax` to check if you can beat min-max.

Replace [`player1` in `TicTacToe.hs`](https://github.com/nikivazou/tic-tac-toe/blob/master/classic/src/TicTacToe.hs#L12) with `playerMinMax` to check if the computer can beat min-max.
