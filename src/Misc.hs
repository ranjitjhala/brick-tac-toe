module Misc where

import System.IO (hFlush, stdout, getLine)

import qualified Control.Monad

foldM :: (Monad m) => (b -> a -> m b) -> b ->  [a] -> m b
foldM = Control.Monad.foldM

prompt :: String -> IO String
prompt s = do
    putStr s
    hFlush stdout
    getLine