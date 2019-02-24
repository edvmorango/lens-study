{-# LANGUAGE TemplateHaskell #-}

-- https://mmhaskell.com/blog/2017/6/12/taking-a-close-look-at-lenses
module PrismEx where

import           Control.Lens

data Task
  = CodingTask String
               Int
  | DrawingTask String
                Int
                Int
                Int
                Double
  | MultipleTask String
                 [Task]
  deriving (Eq, Show)

makePrisms ''Task

codingTask = CodingTask "Coding Task" 8

drawingTask = DrawingTask "Drawing Task" 255 255 255 0.5

multipleTask = MultipleTask "MultipleTask" [codingTask, drawingTask]

getDrawingProduct :: Task -> Maybe (String, Int, Int, Int, Double)
getDrawingProduct t = t ^? _DrawingTask

getOpacity :: Task -> Maybe Double
getOpacity t = t ^? _DrawingTask . _5

--  Id if task is not a DrawingTask
setOpacity :: Double -> Task -> Task
setOpacity o t = t & _DrawingTask . _5 .~ o

setR :: Int -> Task -> Task
setR r t = t & _DrawingTask . _2 .~ r
