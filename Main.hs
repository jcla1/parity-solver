module Main where
import Control.Monad
import ParitySolver

main :: IO ()
main = print . findChoosenPath . liftM2 (:) (return gs) $ findCompletionPath gs
  where
    -- This GameState represents the 4th level of Parity
    -- http://www.abefehr.com/parity/
    gs = GameState (0,0) (Board (3,3) [3,4,3,1,3,2,1,1,2])
