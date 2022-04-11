{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.BChan (newBChan, writeBChan)

import Graphics.Vty.Attributes
import Graphics.Vty.Input
import qualified Graphics.Vty as V

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class

import Control.Concurrent (threadDelay, forkIO)

import qualified Data.Map as M

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
data BoardCell = Space | Brick | Pacman | Ghost deriving (Show, Eq)
type Board = M.Map Position BoardCell
data Movement = MoveLeft | MoveRight | MoveUp | MoveDown | Stop deriving Show

type Ghosts = [Character]

data Character = Character {
  _position :: Position,
  _movement :: Movement
} deriving Show

makeLenses ''Character

data ContextData = ContextData {
  _player :: Character,
  _dashboardSize :: DashBoardSize,
  _dashboard :: Board,
  _ghosts :: Ghosts
} deriving Show

makeLenses ''ContextData

emptyContextData :: ContextData
emptyContextData = ContextData (Character (0, 0) Stop) (0, 0) M.empty []

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

type CharacterL = Lens' ContextData Character

maze :: [String]
maze = [
  "+----------+",
  "| C        |",
  "| --+      |",
  "|   +----- |",
  "|          |",
  "| ------   |",
  "|     M    |",
  "|   ---    |",
  "|  M       |",
  "+----------+"]

parseCell :: Char -> BoardCell
parseCell ' ' = Space
parseCell 'C' = Pacman
parseCell 'M' = Ghost
parseCell _ = Brick

parseRow :: (Row, String) -> ContextData -> ContextData
parseRow (row, line) board = foldr f board $ zip [0..] line
  where f (col, c) = case parseCell c of
                          Pacman -> set (player . position) (row, col) . 
                                    set (cell (row, col)) (Just Space)
                          Ghost  -> over ghosts (Character (row, col) MoveRight:) . 
                                    set (cell (row, col)) (Just Space)
                          x      -> set (cell (row, col)) (Just x)
                               
    
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
          | pos == cd ^. player . position = 'C'
          | cd ^. cell pos == Just Brick = '+'
          | any ((== pos) . view position) (cd ^. ghosts) = 'M'
          | otherwise = ' '
                     
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

safeMove :: Movement -> CharacterL -> ContextData -> ContextData
safeMove move cl cd 
  | cd ^. cell p == Just Space = cd & cl . position .~ p 
  | otherwise = cd & cl . movement .~ Stop 
  where p = (case move of
              MoveUp -> up
              MoveDown -> down
              MoveLeft -> left
              MoveRight -> right
              Stop -> id) $ cd ^. cl . position

processTick :: ContextData -> ContextData
processTick cd =  (movePacman . moveGhosts) cd
  where movePacman = safeMove (cd ^. player . movement) player
        moveGhosts = undefined --TODO: traversed . ghosts

handleEvent :: ContextData -> BrickEvent NameData EventData -> EventM NameData (Next ContextData)
handleEvent cd (AppEvent Tick) = continue $ processTick cd
handleEvent cd (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt cd
handleEvent cd (VtyEvent (EvKey KUp [])) = continue $ set (player . movement) MoveUp cd
handleEvent cd (VtyEvent (EvKey KDown [])) = continue $ set (player . movement) MoveDown cd
handleEvent cd (VtyEvent (EvKey KLeft [])) = continue $ set (player . movement) MoveLeft cd
handleEvent cd (VtyEvent (EvKey KRight [])) = continue $ set (player . movement) MoveRight cd
handleEvent cd _  = continue cd

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
        , appHandleEvent = handleEvent
        , appStartEvent = handleStartEvent
        , appAttrMap = handleAttrMap
      }
      initialState = parseBoard maze
  vty <- V.mkVty V.defaultConfig
  finalState <- customMain vty (V.mkVty V.defaultConfig) (Just chan) app initialState
  return ()