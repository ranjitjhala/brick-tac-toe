module Main where

import Brick
import Graphics.Vty.Attributes
import qualified Graphics.Vty as V
import Brick.BChan (newBChan, writeBChan)
import Control.Monad (forever)
import Control.Concurrent (threadDelay, forkIO)

-- import Control.Monad.IO.Class (liftIO)
-- import Data.Maybe (fromMaybe)


import Model
import View 
import Control 

-------------------------------------------------------------------------------

initState :: PlayState
initState = Model.init 1

main :: IO ()
-- main = defaultMain app initState >>= print . psScore
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- decides how fast your game moves
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  res <- customMain initialVty buildVty (Just chan) app initState
  print (psScore res) 

app :: App PlayState Tick String
app = App
  { appDraw         = view 
  , appChooseCursor = const . const Nothing
  , appHandleEvent  = control 
  , appStartEvent   = return
  , appAttrMap      = const (attrMap defAttr [])
  }

-------------------------------------------------------------------------------
{- 
player1, player2 :: Player
player1 = playerHuman "me"     
player2 = playerComputer

main' :: IO ()
main' = do
  putStrLn "This is classic tic tac toe game."
  rounds  <- prompt "How many rounds should we play?"
  score   <- playRounds (read rounds) player1 player2 
  putStrLn $ showFinalScore score 


playRounds :: Int -> Player -> Player -> IO Score
playRounds rounds player1 player2 = 
  foldM (playRound pi1 pi2) [(pi1,0),(pi2,0)] [1..rounds]
  where 
    pi1 = PI player1 X 1 
    pi2 = PI player2 O 2 

playRound :: PlayerInfo -> PlayerInfo -> Score -> Int -> IO Score 
playRound p1 p2 score i = do 
   putStrLn ("Score:: " ++ showScore score)
   putStrLn ("Round " ++ show i ++ "!")
   putStrLn ((if even i then show p2 else show p1)  ++ " plays first")
   result <- if even i then play p2 p1 emptyBoard else play p1 p2 emptyBoard
   case result of 
      Just p  -> putStrLn (show p ++ " wins!\n\n") >> return (incr p score)
      Nothing -> putStrLn "Its a tie!\n\n" >> return score 


play :: PlayerInfo -> PlayerInfo -> Board -> IO (Maybe Winner)
play pi1@(PI p1 t1 _) pi2 board = do 
  move <- playerMove p1 t1 board
  case putMaybe board t1 move of
    Nothing -> putStrLn "Invalid move." >> return (Just pi2)
    Just b  -> do putStrLn $ showBoard b
                  if tileWins b t1
                    then return (Just pi1) 
                    else if checkFull b 
                    then return Nothing 
                    else play pi2 pi1 b 

-}
-------------------------------------------------------------------------------

