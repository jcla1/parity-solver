module Main where

import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (fromJust)

import Data.Aeson (decode)

import LevelParser
import ParitySolver

main :: IO ()
main = do
    fc <- BS.readFile "story.json"
    let levels = map fromLevel . fromJust $ decode fc
    let findPath gs = findChoosenPath . liftA2 (:) (pure gs) $ findCompletionPath gs

--    print . findPath $ levels !! 12

    print . mapM_ findPath $ take 50 levels
