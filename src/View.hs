module View (view) where

import Brick
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Text.Printf (printf)

import Model
import Model.Board

-------------------------------------------------------------------------------
view :: PlayState -> [Widget String]
-------------------------------------------------------------------------------
view s = [view' s]

view' :: PlayState -> Widget String
view' s = 
  withBorderStyle unicode $
    borderWithLabel (str (header s)) $
      vTile [ mkRow s row | row <- [1..dim] ]

header :: PlayState -> String
header s = printf "Tic-Tac-Toe row = %d, col = %d" (pRow p) (pCol p)
  where p = psPos s

mkRow :: PlayState -> Int -> Widget n
mkRow s row = hTile [ cell s row i | i <- [1..dim] ]

cell :: PlayState -> Int -> Int -> Widget n
cell s r c 
  | isCurr s r c = center (fill '.')
  | otherwise    = center (str (printf "(%d, %d)" r c)) 

vTile :: [Widget n] -> Widget n
vTile (b:bs) = vBox (b : [hBorder <=> b | b <- bs])
vTile _      = emptyWidget

hTile :: [Widget n] -> Widget n
hTile (b:bs) = hBox (b : [vBorder <+> b | b <- bs])
hTile _      = emptyWidget