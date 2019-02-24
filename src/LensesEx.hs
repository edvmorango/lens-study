{-# LANGUAGE TemplateHaskell #-}

module LensesEx where

import           Control.Lens
import           Data.Time

data Task = Task
  { _taskName       :: String
  , _taskMinutes    :: Int
  , _taskFinishTime :: Maybe UTCTime
  }

data Project = Project
  { _projectName        :: String
  , _projectCurrentTask :: Task
  , _projectTasks       :: [Task]
  }

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
