{-# LANGUAGE OverloadedStrings #-}

module LevelParser where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad       (liftM, mzero)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe          (fromJust)
import           Data.Scientific     (coefficient)
import qualified Data.Text           as T
import qualified Data.Vector         as V

import           Data.Aeson          (FromJSON (..), Value (..), (.:), (.:?))

import           ParitySolver        hiding (getBoard, getColors)

data Level = Level { getType         :: String
                     , getID         :: Int
                     , getMode       :: String
                     , getBoard      :: [Int]
                     , getColors     :: Maybe [Color]
                     , getInitialPos :: Position
                     , getSolution   :: Maybe [Direction]
                     } deriving (Show)

fromLevel :: Level -> GameState
fromLevel l = GameState (getInitialPos l) b
  where
    b = case getMode l of
        "b&w" -> BWBoard (fromJust $ getColors l) (getBoard l)
        "vanilla" -> StdBoard (getBoard l)
        x -> error $ "Unknown board type: " ++ x

instance FromJSON Level where
    parseJSON (Object v) = Level
                           <$> v .: "type"
                           <*> v .: "number"
                           <*> v .: "mode"
                           <*> v .: "contents"
                           <*> liftM parseColors (v .:? "colors")
                           <*> liftM parseInitPos (v .: "initialSelected")
                           <*> liftM parseDirection (v .:? "solution")
    parseJSON _ = mzero

parseInitPos :: Value -> Position
parseInitPos (Object ref) = (getField "x", getField "y")
  where
    getField field = case fromJust $ HM.lookup field ref of
        (Number x) -> fromIntegral $ coefficient x
        _ -> 0
parseInitPos _ = error "Invalid initial position"

parseDirection :: Maybe Value -> Maybe [Direction]
parseDirection (Just (Array xs)) = Just . map read $ prepareADTList xs
parseDirection _ = Nothing

parseColors :: Maybe Value -> Maybe [Color]
parseColors (Just (Array xs)) = Just . map read $ prepareADTList xs
parseColors _ = Nothing

prepareADTList :: V.Vector Value -> [String]
prepareADTList = map (\(String x) -> T.unpack (T.toUpper x)) . V.toList
