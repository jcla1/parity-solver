-- This is a representation & solver for the recent "Parity" game.
-- It finds the optimal path through the puzzle via an A* graph search.
-- You can find the game here: http://www.abefehr.com/parity/
-- Feedback would be greatly appreciated!
module ParitySolver where

import Control.Applicative
import Control.Arrow
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
                   getDimensions :: Dimensions
                   , getFields :: [Int]
                   } deriving (Eq, Ord)

instance Show Board where
    show (Board (_, y) xs) = unlines . map show $ chunksOf y xs

data GameState = GameState {
                            position :: Position
                            , getBoard :: Board
                            } deriving (Show, Eq, Ord)

hasGameEnded :: GameState -> Bool
hasGameEnded (GameState _ (Board _ (x:xs))) = all (==x) xs

validBoard :: Board -> Bool
validBoard (Board (dimX, dimY) fields) = dimX*dimY == length fields

applyDirection :: Direction -> GameState -> Maybe GameState
applyDirection dir (GameState pos board) = do
    newPos <- updatePosition (getDimensions board) dir pos
    let newBoard = updateBoard newPos board
    return $ GameState newPos newBoard

updateBoard :: Position -> Board -> Board
updateBoard pos board =
    board { getFields = bumpVal $ getFields board }
  where
    -- TODO: Surely, there must be a better way to do this
    --       than using a lens.
    bumpVal = (& element idx +~ 1)
    idx = convertPosToIndex (getDimensions board) pos

convertPosToIndex :: Dimensions -> Position -> Int
convertPosToIndex (_, dimY) (x, y) = y * dimY + x

findUsedDirection :: Position -> Position -> Direction
findUsedDirection (x1, y1) (x2, y2) = case (x1-x2, y1-y2) of
    (0, 1) -> U
    (0, -1) -> D
    (1, 0) -> L
    (-1, 0) -> R

updatePosition :: Dimensions -> Direction -> Position -> Maybe Position
updatePosition dim dir = validatePosition dim . case dir of
    U -> second (subtract 1)
    D -> second (+1)
    L -> first (subtract 1)
    R -> first (+1)

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
          boardHeuristic
          hasGameEnded

boardHeuristic :: GameState -> Int
boardHeuristic gs = negate . sum $ map diffFn fields
  where
    diffFn = subtract $ maximum fields
    fields = getFields $ getBoard gs

-- For this function to work properly, you need to have the initial
-- GameState prepended.
findChoosenPath :: Maybe [GameState] -> Maybe [Direction]
findChoosenPath Nothing = Nothing
findChoosenPath (Just gs) = Just $ zipWith findUsedDirection ps (tail ps)
  where
    ps = map position gs
