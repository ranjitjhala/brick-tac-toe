module Control where

import Brick
import qualified Graphics.Vty as V
import qualified Brick.Types as T

import Model
import Model.Board

-------------------------------------------------------------------------------

control :: PlayState -> BrickEvent n e -> EventM n (Next PlayState)
control s ev = case ev of 
  T.VtyEvent (V.EvKey V.KUp   _)  -> Brick.continue (move up    s)
  T.VtyEvent (V.EvKey V.KDown _)  -> Brick.continue (move down  s)
  T.VtyEvent (V.EvKey V.KLeft _)  -> Brick.continue (move left  s)
  T.VtyEvent (V.EvKey V.KRight _) -> Brick.continue (move right s)
  T.VtyEvent (V.EvKey V.KEnter _) -> Brick.continue (play   s)
  _                               -> Brick.halt s

-------------------------------------------------------------------------------

play :: PlayState -> PlayState
play = error "TODO: play"

move :: (Pos -> Pos) -> PlayState -> PlayState
move f s = s { psPos = f (psPos s) }