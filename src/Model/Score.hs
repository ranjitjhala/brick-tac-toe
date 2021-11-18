{-# LANGUAGE RecordWildCards #-}
module Model.Score where

import Model.Board (Result (..), XO (..))

-------------------------------------------------------------------------------
-- | Score --------------------------------------------------------------------
-------------------------------------------------------------------------------

data Score = Score 
  { scMax  :: Int  -- ^ total number of boards
  , scX    :: Int  -- ^ points for player X 
  , scO    :: Int  -- ^ points for player O 
  , scD    :: Int  -- ^ drawn games 
  }
  deriving (Eq, Ord, Show)

init :: Int -> Score
init n = Score n 0 0 0

add :: Score -> Maybe XO -> Score
add sc (Just X) = sc { scX = scX sc + 1 }
add sc (Just O) = sc { scO = scO sc + 1 }
add sc Nothing  = sc { scD = scD sc + 1 }

get :: Score -> XO -> Int
get Score {..} X = scX 
get Score {..} O = scO 

currRound :: Score -> Int
currRound Score {..} = scX + scO + scD + 1

startPlayer :: Score -> XO
startPlayer sc 
  | even (currRound sc) = X
  | otherwise           = O

winner :: Score -> Result () 
winner sc@Score {..}
  | scX > scO + left = Win X
  | scO > scX + left = Win O
  | left == 0        = Draw
  | otherwise        = Cont ()
  where 
    left             = 1 + scMax - currRound sc