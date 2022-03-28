{-# LANGUAGE TemplateHaskell #-}

module Main where

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

import Graphics.Vty.Attributes
import Graphics.Vty.Input

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class

import qualified Data.Map as M

type NameData = ()
type EventData = ()

type Row = Int
type Column = Int

type Position = (Row, Column)
type DashBoardSize = (Int, Int)

data BoardCell = Space | Brick | Pacman deriving Show
type Board = M.Map Position BoardCell

data ContextData = ContextData {
  _position :: Position,
  _dashboardSize :: DashBoardSize,
  _dashboard :: Board
} deriving Show

makeLenses ''ContextData

emptyContextData :: ContextData
emptyContextData = ContextData (0, 0) (0, 0) M.empty

positionX :: Lens' ContextData Int
positionX = position . _1

positionY :: Lens' ContextData Int
positionY = position . _2

width :: Lens' ContextData Int
width = dashboardSize . _1

height :: Lens' ContextData Int
height = dashboardSize . _2

maze :: [String]
maze = [
  "+--------+",
  "|C       |",
  "| --+    |",
  "|   +--- |",
  "|        |",
  "| ------ |",
  "|        |",
  "|   ---  |",
  "|        |",
  "+--------+"]

parseCell :: Char -> Maybe BoardCell
parseCell ' ' = Just Space
parseCell 'C' = Nothing
parseCell _ = Just Brick

parseRow :: (Row, String) -> ContextData -> ContextData
parseRow (row, line) board = foldr f board $ zip [0..] line
  where f (col, cell) = case parseCell cell of
                          Just x -> over dashboard $ M.insert (row, col) x 
                          Nothing -> set position (col, row)
    
parseBoard :: [String] -> ContextData
parseBoard rows = set width (length $ head rows)
  . set height (length rows)
  . foldr parseRow emptyContextData 
  $ zip [0..] rows

drawBoard :: ContextData -> Widget NameData
drawBoard cd = border $ vBox rows
  where rows = [str $ cellsInRow r | r <- [0 .. cd ^. height - 1]]        
        cellsInRow r = do
          c <- [0 .. cd ^. width - 1]
          return $ if c == cd ^. positionX && r == cd ^. positionY then 
            'C' 
          else 
            case M.lookup (r, c) (cd ^. dashboard) of
              Just Brick -> '+'
              _ -> ' '

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
  
handleEvent :: ContextData -> BrickEvent NameData EventData -> EventM NameData (Next ContextData)
handleEvent cd (VtyEvent (EvKey (KChar 'c') [MCtrl]))  = halt cd
handleEvent cd (VtyEvent (EvKey KUp [])) = continue $
  cd & positionY %~ safeDecrease
handleEvent cd (VtyEvent (EvKey KDown [])) = continue $ 
  cd & positionY %~ safeIncrease (cd ^. height)
handleEvent cd (VtyEvent (EvKey KLeft [])) = continue $
  cd & positionX %~ safeDecrease
handleEvent cd (VtyEvent (EvKey KRight [])) = continue $
  cd & positionX %~ safeIncrease (cd ^. width)
handleEvent cd _  = continue cd 

handleStartEvent :: ContextData -> EventM NameData ContextData
handleStartEvent = return

handleAttrMap :: ContextData -> AttrMap
handleAttrMap s = attrMap defAttr []

main :: IO ()
main = do
  let app = App { 
        appDraw = handleDraw
        , appChooseCursor = neverShowCursor
        , appHandleEvent = handleEvent
        , appStartEvent = handleStartEvent
        , appAttrMap = handleAttrMap
      }
      initialState = parseBoard maze
  finalState <- defaultMain app initialState
  return ()