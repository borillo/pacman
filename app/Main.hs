{-# LANGUAGE TemplateHaskell #-}

module Main where

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

import Graphics.Vty.Attributes
import Graphics.Vty.Input

import Control.Lens

type NameData = ()
type EventData = ()

data ContextData = ContextData {
  _eventCount :: Int,
  _lastEvent :: String
} deriving (Show)

makeLenses ''ContextData

ui :: ContextData -> Widget NameData
ui cd = withBorderStyle unicode $
        border (center (str . show $ cd ^. eventCount) <+> vBorder <+> center (str $ cd ^. lastEvent))

handleDraw :: ContextData -> [Widget NameData]
handleDraw s = [ui s]

handleEvent :: ContextData -> BrickEvent NameData EventData -> EventM NameData (Next ContextData)
handleEvent cd (VtyEvent (EvKey (KChar 'c') [MCtrl]))  = halt cd
handleEvent cd e@(VtyEvent (EvKey _ _))  = continue $ 
  cd & eventCount %~ (+1)
     & lastEvent .~ show e
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
      initialState = ContextData 0 "init"
  finalState <- defaultMain app initialState
  return ()