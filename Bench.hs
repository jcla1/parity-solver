import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Maybe                 (fromJust)

import           Criterion.Main
import           Data.Aeson                 (decode)

import           LevelParser
import           ParitySolver

main :: IO ()
main = do
    fc <- BS.readFile "story.json"
    let levels = map fromLevel . fromJust $ decode fc
    let findPath gs = findChoosenPath $ findCompletionPath gs

    let first50Levels = take 50 levels
    let last50Levels = drop 50 levels
    let eval = mapM_ findPath

    defaultMain [
        bench "first 50" $ whnf eval first50Levels
        , bench "last 50" $ whnf eval last50Levels
                ]
