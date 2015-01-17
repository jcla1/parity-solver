-- This is a representation & solver for the recent "Parity" game.
-- It finds the optimal path through the puzzle via an A* graph search.
-- You can find the game here: http://www.abefehr.com/parity/
-- Feedback would be greatly appreciated!
module ParitySolver where

import Control.Applicative
import Control.Lens
import Data.Graph.AStar
import Data.List.Split
import Data.Maybe
import qualified Data.Set as S

data Direction = U | D | L | R
    deriving (Show, Read, Eq, Enum)

data Color = White | Black
    deriving (Show, Read, Eq)

-- (0, 0) is the top left of the board
type Position = (Int, Int)
type Dimensions = (Int, Int)

-- The data runs in rows
data Board = Board {
                   dimensions :: Dimensions
                   , fields :: [Int]
                   } deriving (Eq, Ord)

instance Show Board where
    show (Board (_, y) xs) = unlines . map show $ chunksOf y xs

data GameState = GameState {
                            position :: Position
                            , board :: Board
                            } deriving (Show, Eq, Ord)

hasGameEnded :: GameState -> Bool
hasGameEnded (GameState _ (Board _ xs)) = all (== head xs) (tail xs)

validBoard :: Board -> Bool
validBoard (Board (dimX, dimY) fields) = dimX*dimY  == length fields

applyDirection :: Direction -> GameState -> Maybe GameState
applyDirection dir (GameState pos board) = do
    newPos <- updatePosition (dimensions board) dir pos
    let newBoard = updateBoard newPos board
    return $ GameState newPos newBoard

updateBoard :: Position -> Board -> Board
updateBoard pos board =
    board { fields = bumpVal $ fields board }
  where
    -- TODO: Surely, there must be a better way to do this
    --       than using a lens.
    bumpVal = (& element idx +~ 1)
    idx = convertPosToIndex (dimensions board) pos

convertPosToIndex :: Dimensions -> Position -> Int
convertPosToIndex (_, dimY) (x, y) = y * dimY + x

updatePosition :: Dimensions -> Direction -> Position -> Maybe Position
updatePosition dim dir (x, y) = validatePosition dim $ case dir of
    U -> (x, y-1)
    D -> (x, y+1)
    L -> (x-1, y)
    R -> (x+1, y)

validatePosition :: Dimensions -> Position -> Maybe Position
validatePosition (dimX, dimY) (x, y)
    | x < 0 || x >= dimX || y < 0 || y >= dimY= Nothing
    | otherwise = Just (x, y)

findNeighbours :: GameState -> [GameState]
findNeighbours gs = catMaybes $ applyDirection <$> [U .. R] <*> [gs]

findCompletionPath :: GameState -> Maybe [GameState]
findCompletionPath =
    aStar (S.fromList . findNeighbours)
          (const . const 1)
          (const 0)
          hasGameEnded

main :: IO ()
main = print . fromJust $ findCompletionPath gs
  where
    -- This GameState represents the 4th level of Parity
    -- http://www.abefehr.com/parity/
    gs = GameState (0,0) (Board (3,3) [3,4,3,1,3,2,1,1,2])
