module Player.Computer (playerComputer) where 

import System.Random (randomRIO)
import Prelude hiding ((!!))

import Types 

playerComputer :: Player
playerComputer = Player getRandomMove "Computer" 


getRandomMove :: Tile -> Board -> IO (Int, Int)
getRandomMove t b = do
    col <- randomRIO (1,3)
    row <- randomRIO (1,3)
    case b!!(row, col) of
        EmptyTile -> return (row, col)
        _         -> getRandomMove t b