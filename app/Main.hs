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
data BoardCell = Space | Brick | Pacman deriving (Show, Eq)
type Board = M.Map Position BoardCell
data Movement = MoveLeft | MoveRight | MoveUp | MoveDown | Stop deriving Show

data ContextData = ContextData {
  _position :: Position,
  _movement :: Movement,
  _dashboardSize :: DashBoardSize,
  _dashboard :: Board
} deriving Show

makeLenses ''ContextData

emptyContextData :: ContextData
emptyContextData = ContextData (0, 0) Stop (0, 0) M.empty

positionX :: Lens' ContextData Column
positionX = position . _2

positionY :: Lens' ContextData Row
positionY = position . _1

width :: Lens' ContextData Width
width = dashboardSize . _1

height :: Lens' ContextData Height
height = dashboardSize . _2

cell :: Position -> Lens' ContextData (Maybe BoardCell)
cell p = dashboard . at p

maze :: [String]
maze = [
  "+----------+",
  "| C        |",
  "| --+      |",
  "|   +----- |",
  "|          |",
  "| ------   |",
  "|          |",
  "|   ---    |",
  "|          |",
  "+----------+"]

parseCell :: Char -> Maybe BoardCell
parseCell ' ' = Just Space
parseCell 'C' = Nothing
parseCell _ = Just Brick

parseRow :: (Row, String) -> ContextData -> ContextData
parseRow (row, line) board = foldr f board $ zip [0..] line
  where f (col, c) = case parseCell c of
                          Just x -> set (cell (row, col)) (Just x)
                          Nothing -> set position (row, col) . 
                                     set (cell (row, col)) (Just Space)
    
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
          | pos == cd ^. position = 'C'
          | cd ^. cell pos == Just Brick = '+'
          | otherwise = ' '
                     
ui :: ContextData -> Widget NameData
ui cd = withBorderStyle unicode $ drawBoard cd

handleDraw :: ContextData -> [Widget NameData]
handleDraw s = [ui s]

safeDecrease :: Int -> Int
safeDecrease n
  | n > 0 = n - 1
  | otherwise = 0                                                                    
  │++++++++++++│                       

safeIncrease :: Int -> Int -> Int
safeIncrease limit n
  | n < limit - 1 = n + 1
  | otherwise = limit - 1

safeMove :: Movement -> ContextData -> ContextData
safeMove move cd 
  | cd ^. cell p == Just Space = cd & position .~ p 
  | otherwise = cd & movement .~ Stop 
  where p = (case move of
              MoveUp -> up
              MoveDown -> down
              MoveLeft -> left
              MoveRight -> right
              Stop -> id) $ cd ^. position

handleEvent :: ContextData -> BrickEvent NameData EventData -> EventM NameData (Next ContextData)
handleEvent cd (AppEvent Tick) = continue $ safeMove (cd ^. movement) cd
handleEvent cd (VtyEvent (EvKey (KChar 'c') [MCtrl])) = halt cd
handleEvent cd (VtyEvent (EvKey KUp [])) = continue $ set movement MoveUp cd
handleEvent cd (VtyEvent (EvKey KDown [])) = continue $ set movement MoveDown cd
handleEvent cd (VtyEvent (EvKey KLeft [])) = continue $ set movement MoveLeft cd
handleEvent cd (VtyEvent (EvKey KRight [])) = continue $ set movement MoveRight cd
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