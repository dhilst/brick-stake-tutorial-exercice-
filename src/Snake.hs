{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}

module Snake where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Data.Sequence (Seq, ViewL(..), ViewR(..), (<|))
import qualified Data.Sequence as S
import Lens.Micro ((%~), (&), (.~), (^.))
import Lens.Micro.TH (makeLenses)
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), newStdGen)

-- Types
data Game =
  Game
    { _snake :: Snake -- ^ snake as a sequence of points in R2
    , _dir :: Direction -- ^ direction
    , _food :: Coord -- ^ location of the food
    , _foods :: Stream Coord -- ^ infinite list of random food locations
    , _dead :: Bool -- ^ game over flag
    , _paused :: Bool -- ^ paused flag
    , _score :: Int -- ^ score
    , _frozen :: Bool -- ^ freeze to disallow duplicate turns
    }
  deriving (Show)

type Coord = V2 Int

type Snake = Seq Coord

data Stream a =
  a :| Stream a
  deriving (Show)

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

step :: Game -> Game
step g =
  fromMaybe g $ do
    guard (not $ g ^. paused || g ^. dead)
    let g' = g & frozen .~ False
    return . fromMaybe (move g') $ die g' <|> eatFood g'

die :: Game -> Maybe Game
die = undefined

eatFood :: Game -> Maybe Game
eatFood = undefined

move :: Game -> Game
move = undefined

turn :: Direction -> Game -> Game
turn = undefined

initGame :: IO Game
initGame = undefined
