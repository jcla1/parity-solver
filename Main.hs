module Main where

import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS

import Data.Aeson (decode)

import LevelParser
import ParitySolver

main :: IO ()
main = do
    fc <- BS.readFile "story.json"
    let path = findChoosenPath . findCompletionPath . fromLevel . head =<< decode fc
    print path
