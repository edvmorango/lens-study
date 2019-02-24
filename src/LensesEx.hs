{-# LANGUAGE TemplateHaskell #-}

--https://mmhaskell.com/blog/2017/6/12/taking-a-close-look-at-lenses
module LensesEx where

import           Control.Lens
import           Data.Time

data Task = Task
  { _taskName       :: String
  , _taskMinutes    :: Int
  , _taskFinishTime :: Maybe UTCTime
  } deriving (Eq, Show)

data Project = Project
  { _projectName        :: String
  , _projectCurrentTask :: Task
  , _projectTasks       :: [Task]
  } deriving (Eq, Show)

makeLenses ''Task

makeLenses ''Project

defaultTask =
  Task {_taskName = "Get", _taskMinutes = 100, _taskFinishTime = Nothing}

defaultProject =
  Project
    { _projectName = "Lens Study"
    , _projectCurrentTask = defaultTask
    , _projectTasks = [defaultTask]
    }

getTaskName :: Task -> String
getTaskName task = task ^. taskName

setTaskName :: String -> Task -> Task
setTaskName n t = t & taskName .~ n

--  (.~) :: lensFunction -> newValue -> value
--  lensFunction -> value -> Lens' a b
setTaskName' :: String -> Task -> Task
setTaskName' n t = (.~) taskName n t

increaseTaskMinutes :: Int -> Task -> Task
increaseTaskMinutes m t = t & taskMinutes %~ (+ m)
