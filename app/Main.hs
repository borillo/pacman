{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Brick hiding (Direction)
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.BChan (newBChan, writeBChan)

import Graphics.Vty.Attributes
import Graphics.Vty.Input
import Graphics.Vty qualified as V

import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class

import Control.Concurrent (threadDelay, forkIO)

import Data.Maybe(catMaybes, isJust, fromJust)
import Data.List(find)
import Data.Map qualified as M
import Data.Set qualified as S

import System.Random

type NameData = ()
data EventData = Tick

type Row = Int
type Column = Int
type Width = Int
type Height = Int

type Position = (Row, Column)

up :: Position -> Position
up p = p & _1 %~ subtract 1

down :: Position -> Position
down p = p & _1 %~ (+1)

left :: Position -> Position
left p = p & _2 %~ subtract 1

right :: Position -> Position
right p = p & _2 %~ (+1)

type DashBoardSize = (Width, Height)
data BoardCell = Space | Brick Char deriving (Show, Eq)
data Sprite = Pacman | Dot | Ghost Strategy Name deriving (Show, Eq)
type Board = M.Map Position BoardCell
data Movement = Moving Direction | Stop deriving (Show, Eq, Ord)
data Direction = DLeft | DRight | DUp | DDown deriving (Show, Eq, Ord, Enum)
type Name = Char

directions :: [Direction]
directions = enumFrom DLeft

type Ghosts = [Character]
type Strategy = Character -> PacmanM Character

instance Show Strategy where
  show s = "Strategy()"

instance Eq Strategy where
  s1 == s = False

turnLeft :: Direction -> Direction
turnLeft DLeft  = DDown
turnLeft DRight = DUp
turnLeft DUp    = DLeft
turnLeft DDown  = DRight

turnRight :: Direction -> Direction
turnRight DLeft  = DUp
turnRight DRight = DDown
turnRight DUp    = DRight
turnRight DDown  = DLeft

move :: Direction -> Position -> Position
move DUp    = up
move DDown  = down
move DLeft  = left
move DRight = right

type PacmanT = StateT ContextData
type PacmanM = PacmanT Identity
type PacmanEvent = PacmanT (EventM NameData)

data Character = Character {
  _position :: Position,
  _movement :: Movement,
  _changeDirection :: Strategy,
  _name :: Name
} deriving Show

data ContextData = ContextData {
  _player :: Character,
  _dashboardSize :: DashBoardSize,
  _dashboard :: Board,
  _ghosts :: Ghosts,
  _dots :: S.Set Position,
  _randomGenerator :: StdGen
} deriving Show

makeLenses ''Character
makeLenses ''ContextData

doNothing :: Strategy
doNothing = return

distance :: Position -> Position -> Int
distance (row, col) (row', col') = (row - row')^2 + (col - col')^2

distanceToPacman :: Position -> PacmanM Int
distanceToPacman p = uses (player . position) (distance p)

chooseOne :: [a] -> PacmanM a
chooseOne xs = do
  (i, g) <- uses randomGenerator (randomR (0, length xs-1))
  randomGenerator .= g
  return $ xs !! i

doChasePacman :: Strategy
doChasePacman c = do
  freeDirections <- filterM (\d -> isFree $ move d (c ^. position)) directions
  ds <- mapM (\d -> do
    dst <- distanceToPacman $ move d (c ^. position)
    return (dst, d)) freeDirections
  let closest = snd $ minimum ds
  return $ set movement (Moving closest) c

doRandomStrategy :: Strategy
doRandomStrategy c = do
  freeDirections <- filterM (\d -> isFree $ move d (c ^. position)) directions
  newMovement <- chooseOne freeDirections
  return $ set movement (Moving newMovement) c

onCD :: Monad m => (ContextData -> m b) -> PacmanT m b
onCD f = get >>= lift . f

withPacmanM :: Monad m => PacmanM a -> PacmanT m a
withPacmanM = mapStateT $ return . runIdentity

emptyContextData :: ContextData
emptyContextData = ContextData {
  _player = Character (0, 0) Stop doNothing 'C',
  _dashboardSize = (0, 0),
  _dashboard = M.empty,
  _ghosts = [],
  _dots = S.empty,
  _randomGenerator = mkStdGen 42
}

positionX :: Lens' Character Column
positionX = position . _2

positionY :: Lens' Character Row
positionY = position . _1

width :: Lens' ContextData Width
width = dashboardSize . _1

height :: Lens' ContextData Height
height = dashboardSize . _2

cell :: Position -> Lens' ContextData (Maybe BoardCell)
cell p = dashboard . at p

isFree :: Monad m => Position -> PacmanT m Bool
isFree p = (== Just Space) <$> use (cell p)

type CharacterL = Lens' ContextData Character

maze :: [String]
maze = [
  "+==========+",
  "|·C···====·|",
  "|·==+······|",
  "|···+=====·|",
  "|·|········|",
  "|·+==·====·|",
  "|·····W····|",
  "|·==·===·=·|",
  "|··M·······|",
  "+==========+"]

parseCell :: Char -> Either BoardCell Sprite
parseCell ' ' = Left Space
parseCell '·' = Right Dot
parseCell 'C' = Right Pacman
parseCell 'M' = Right $ Ghost doChasePacman 'M'
parseCell 'W' = Right $ Ghost doRandomStrategy 'W'
parseCell x = Left $ Brick x

parseRow :: (Row, String) -> ContextData -> ContextData
parseRow (row, line) board = foldr f board $ zip [0..] line
  where f (col, c) =
          case parseCell c of
               Left bc  -> setCell bc
               Right sp -> setCell Space . setSprite sp
          where p = (row, col)
                setCell :: BoardCell -> ContextData -> ContextData
                setCell = set (cell p) . Just
                setSprite :: Sprite -> ContextData -> ContextData
                setSprite = \case
                      Dot       -> over dots (S.insert p)
                      Pacman    -> set (player . position) p
                      Ghost s n -> over ghosts (Character p (Moving DRight) s n :)

parseBoard :: [String] -> ContextData
parseBoard rows = set width (length $ head rows)
  . set height (length rows)
  . foldr parseRow emptyContextData
  $ zip [0..] rows

drawBoard :: ContextData -> Widget NameData
drawBoard cd = border $ vBox rows
  where rows = map (str . cellsInRow) [0 .. cd ^. height - 1]
        cellsInRow row = map (cellToChar . (row,)) [0 .. cd ^. width - 1]
        cellToChar pos
          | pos == cd ^. player . position = cd ^. player . name
          | isJust firstPos = fromJust firstPos ^. name
          | S.member pos (cd ^. dots) = '·'
          | otherwise = case cd ^. cell pos of
              Just (Brick c) -> c
              _ -> ' '
          where firstPos = find ((== pos) . view position) (cd ^. ghosts)

ui :: ContextData -> Widget NameData
ui cd = withBorderStyle unicode $ drawBoard cd

handleDraw :: ContextData -> [Widget NameData]
handleDraw s = [ui s]

safeDecrease :: Int -> Int
safeDecrease n
  | n > 0 = n - 1
  | otherwise = 0

safeIncrease :: Int -> Int -> Int
safeIncrease limit n
  | n < limit - 1 = n + 1
  | otherwise = limit - 1

canRotate :: Position -> Direction -> PacmanM Bool
canRotate p d = (||) <$> isFree (move (turnLeft d) p)
                     <*> isFree (move (turnRight d) p)

safeMove :: Character -> PacmanM Character
safeMove ch = case ch ^. movement of
    Stop -> return ch
    Moving d -> do
      let p = move d (ch ^. position)
      free <- isFree p
      if free then
        do
          let ch' = ch & position .~ p
          r <- canRotate p d
          if r then
            (ch' ^. changeDirection) ch'
          else
            return ch'
      else
        return $ ch & movement .~ Stop

eatDot :: PacmanM ()
eatDot = do
  p <- use (player . position)
  dots %= S.delete p

processTick :: PacmanM ()
processTick = do
  player <~ (use player >>= safeMove)
  eatDot
  gs <- use ghosts
  ghosts <~ mapM safeMove gs

evalPacman :: Monad m => PacmanT m a -> ContextData -> m a
evalPacman = evalStateT

movePacman :: Monad m => Direction -> PacmanT m ()
movePacman d = do
  p <- uses (player . position) (move d)
  free <- isFree p
  when free $ assign (player . movement) (Moving d)

handleArrowKey :: Direction -> PacmanEvent (Next ContextData)
handleArrowKey d = do
  movePacman d
  onCD continue

arrows = M.fromList [(KUp, DUp), (KDown, DDown), (KLeft, DLeft), (KRight, DRight)]
handleEvent :: BrickEvent NameData EventData -> PacmanEvent (Next ContextData)
handleEvent (AppEvent Tick) = do
  withPacmanM processTick
  onCD continue
handleEvent (VtyEvent (EvKey (KChar 'c') [MCtrl])) = onCD halt
handleEvent (VtyEvent (EvKey k [])) | k `M.member` arrows = handleArrowKey $ arrows M.! k
handleEvent _  = onCD continue

handleStartEvent :: ContextData -> EventM NameData ContextData
handleStartEvent = return

handleAttrMap :: ContextData -> AttrMap
handleAttrMap s = attrMap defAttr []

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 500000
  let app = App {
        appDraw = handleDraw
        , appChooseCursor = neverShowCursor
        , appHandleEvent = flip (evalPacman . handleEvent)
        , appStartEvent = handleStartEvent
        , appAttrMap = handleAttrMap
      }
      initialState = parseBoard maze
  vty <- V.mkVty V.defaultConfig
  finalState <- customMain vty (V.mkVty V.defaultConfig) (Just chan) app initialState
  return ()
