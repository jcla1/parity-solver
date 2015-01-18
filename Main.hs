module Main where
import Data.Maybe
import ParitySolver

main :: IO ()
main = print . fromJust $ findCompletionPath gs
  where
    -- This GameState represents the 4th level of Parity
    -- http://www.abefehr.com/parity/
    gs = GameState (0,0) (Board (3,3) [3,4,3,1,3,2,1,1,2])
