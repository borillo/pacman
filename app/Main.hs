{-# LANGUAGE TemplateHaskell #-}

module Main where

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

import Graphics.Vty.Attributes
import Graphics.Vty.Input

import Control.Lens
import Control.Monad.IO.Class

type NameData = ()
type EventData = ()

type Position = (Int, Int)
type DashBoardSize = (Int, Int)

data ContextData = ContextData {
  _position :: Position,
  _dashboardSize :: DashBoardSize
} deriving (Show)

makeLenses ''ContextData

width=100
height=10

positionX = position . _1
positionY = position . _2

drawBoard :: ContextData -> Widget NameData
drawBoard cd = border $ vBox rows
  where rows = [str $ cellsInRow r | r <- [0 .. height-1]]        
        spaces w = replicate w ' '
        emptyLine = spaces width
        cellsInRow r 
          | r /= cd ^. positionY = emptyLine
          | otherwise = spaces (cd ^. positionX) ++ 'X':spaces (width - cd ^. positionX - 1)

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
  cd & positionY %~ safeIncrease height
handleEvent cd (VtyEvent (EvKey KLeft [])) = continue $
  cd & positionX %~ safeDecrease
handleEvent cd (VtyEvent (EvKey KRight [])) = continue $
  cd & positionX %~ safeIncrease width
handleEvent cd (VtyEvent (EvResize w h)) = continue $
  cd & dashboardSize .~ (w, h)
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
      initialState = ContextData (0, 0) (width, height)
  finalState <- defaultMain app initialState
  return ()

  -- TODO: Al definir positionX/positionY solo funciona si está 
  -- en drawBoard o en handleEvent, pero no en ambas