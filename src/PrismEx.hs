{-# LANGUAGE TemplateHaskell #-}

-- https://mmhaskell.com/blog/2017/6/12/taking-a-close-look-at-lenses
module PrismEx where

import           Control.Lens

data Task
  = CodingTask String
  | DrawingTask String
                Int
                Int
                Int
                Double
  | MultipleTask String
                 [Task]
  deriving (Eq, Show)

makePrisms ''Task

codingTask = CodingTask "Coding Task"

drawingTask = DrawingTask "Drawing Task" 255 255 255 0.5

multipleTask = MultipleTask "MultipleTask" [codingTask, drawingTask]

getDrawingProduct :: Task -> Maybe (String, Int, Int, Int, Double)
getDrawingProduct t = t ^? _DrawingTask

getOpacity :: Task -> Maybe Double
getOpacity t = t ^? _DrawingTask . _5
