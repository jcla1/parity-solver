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
    deriving (Show, Read, Eq, Ord, Enum)

data Color = W | B
    deriving (Show, Read, Eq, Ord)

-- (0, 0) is the top left of the board
type Position = (Int, Int)
type Dimensions = (Int, Int)

-- The data runs in rows
data Board = StdBoard { getFields :: [Int] }
             | BWBoard { getColors :: [Color]
                         , getFields :: [Int]
                         } deriving (Eq, Ord)

getDimensions :: Board -> Dimensions
getDimensions _ = (3,3)

hasGameEnded :: Board -> Bool
hasGameEnded b = all (==x) xs
    where (x:xs) = getFields b

updateBoard :: Position -> Board -> Board
updateBoard pos board =
    board { getFields = bumpVal $ getFields board }
  where
    idx = convertPosToIndex (getDimensions board) pos
    -- TODO: Surely, there must be a better way to do this
    --       than using a lens.
    bumpVal = case board of
              StdBoard _ -> (& element idx +~ 1)
              BWBoard colrs _ -> case colrs !! idx of
                                 W -> (& element idx +~ 1)
                                 B-> (& element idx -~ 1)

boardHeuristic :: Board -> Int
boardHeuristic b = negate . sum $ map diffFn fields
  where
    diffFn = subtract $ maximum fields
    fields = getFields b

instance Show Board where
    show b = unlines . map show $ chunksOf y xs
      where
        xs = getFields b
        (_,y) = getDimensions b

data GameState = GameState { position :: Position
                             , getBoard :: Board
                             } deriving (Show, Eq, Ord)

applyDirection :: Direction -> GameState -> Maybe GameState
applyDirection dir (GameState pos board) = do
    newPos <- updatePosition (getDimensions board) dir pos
    let newBoard = updateBoard newPos board
    return $ GameState newPos newBoard

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
findCompletionPath gs = liftA2 (:) (pure gs)
    $ aStar (S.fromList . findNeighbours)
            (const . const 1)
            (boardHeuristic . getBoard)
            (hasGameEnded . getBoard)
            gs

findChoosenPath :: Maybe [GameState] -> Maybe [Direction]
findChoosenPath Nothing = Nothing
findChoosenPath (Just gs) = Just $ zipWith findUsedDirection ps (tail ps)
    where ps = map position gs
