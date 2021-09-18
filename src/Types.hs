module Types where 

import Prelude hiding ((!!))

import qualified Data.List 
import qualified Data.Maybe as M 

-------------------------------------------------------------------------------
--- Board ---------------------------------------------------------------------
-------------------------------------------------------------------------------


data Tile = EmptyTile | X | O 
  deriving (Eq)

flipTile :: Tile -> Tile
flipTile X = O 
flipTile O = X 
flipTile _ = EmptyTile

type Move   = (Int,Int)

type Board  = [(Move, Tile)] 

(!!) :: Board -> Move -> Tile
b!!ij = M.fromMaybe EmptyTile (lookup ij b) 

emptyBoard :: Board
emptyBoard = [((x,y), EmptyTile) | x <- [1..3], y <- [1..3]]

validMoves :: Board -> [Move]
validMoves board  = [ij | (ij, EmptyTile) <- board]

put :: Board -> Tile -> Move -> Board
put b t move = M.fromJust $ putMaybe b t move

putMaybe :: Board -> Tile -> Move -> Maybe Board
putMaybe b t xy = case b!!xy of
               EmptyTile -> Just $ map (\(ij,tij) -> if ij == xy then (ij,t) else (ij,tij)) b 
               _         -> Nothing

-------------------------------------------------------------------------------
--- Player --------------------------------------------------------------------
-------------------------------------------------------------------------------

data Player = 
  Player { playerMove :: Tile -> Board -> IO Move
         , playerName :: String
         } 

-------------------------------------------------------------------------------
--- Score ---------------------------------------------------------------------
-------------------------------------------------------------------------------

type Score = [(PlayerInfo, Int)]

showFinalScore :: Score -> String
showFinalScore [(p1,i1),(p2,i2)]
  = if i1 == i2 
      then "Its a tie!" 
      else ("The winner is " ++ show (if i1 < i2 then p2 else p1))

showScore [(p1,i1),(p2,i2)] 
  = show p1 ++ " : " ++ show i1 ++ " VS. " ++ show p2 ++ " : " ++ show i2 
showScore _ 
  = ""

incr :: PlayerInfo -> Score -> Score
incr pi xs = map (\(pj,sj) -> if pi == pj then (pj,sj+1) else (pj,sj)) xs 

-------------------------------------------------------------------------------
--- Player Info ---------------------------------------------------------------
-------------------------------------------------------------------------------

data PlayerInfo =  
  PI { playerInfoPlayer :: Player
     , playerInfoTile   :: Tile
     , playerInfoInt    :: Int
     }

type Winner = PlayerInfo

instance Eq PlayerInfo where
    p1 == p2 = playerInfoInt p1 == playerInfoInt p2 

instance Show Player where
  show = playerName

instance Show PlayerInfo where
  show pi 
    | pname /= "Computer" && pname /= "Human"
    =  pname 
    | otherwise 
    = "Player " ++ show (playerInfoInt pi) 
    where pname = playerName $ playerInfoPlayer pi


instance Show Tile where
  show EmptyTile = "     "
  show X         = "  X  "
  show O         = "  O  "

showBoard :: Board -> String
showBoard b = let blist = boardAsList b
              in  unlines [Data.List.intercalate "|" row | row <- blist]
              where
                boardAsList b = [[show (b!!(x,y)) | y <- [1,2,3]] | x <- [1,2,3]]

showTileNumbers :: String
showTileNumbers  = (unlines
                   [Data.List.intercalate "|" ["(" ++ show x ++ "," ++ show y ++ ")" |
                   y <- [1,2,3]] | x <- [1,2,3]])
