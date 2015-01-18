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
    let levels = decode fc :: Maybe [Level]
    let l = fromJust $ (!!) <$> levels <*> pure 5
    let path = findChoosenPath . findCompletionPath $ fromLevel l
    print path
