module Model.Score where

-------------------------------------------------------------------------------
-- | Score --------------------------------------------------------------------
-------------------------------------------------------------------------------

data Score = Score 
  { scMax :: Int  -- ^ total number of boards
  , scX   :: Int  -- ^ points for player X 
  , scO   :: Int  -- ^ points for player O 
  }
  deriving (Eq, Ord, Show)

init :: Int -> Score
init n = Score n 0 0

addX :: Score -> Score
addX sc = sc { scX = scX sc + 1 }

addO :: Score -> Score
addO sc = sc { scO = scO sc + 1 }

