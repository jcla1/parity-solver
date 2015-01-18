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
    let levels = fromJust (decode fc :: Maybe [Level])
    let findPath = findChoosenPath . findCompletionPath . fromLevel

--    let l = (!!) <$> levels <*> pure 5
--    print $ findPath l

    print $ mapM_ findPath $ take 50 levels
