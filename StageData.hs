module StageData (Id, Name, Class, Instance, Stats, World(..), Action(..), Thing(..)) where

import qualified Data.Map.Strict as Map
import GHC.Natural

type Id = String
type Name = String
type Class = String
type Instance = Natural
type Stats = Map.Map String Integer

data World = World { things :: Map.Map Id Thing
                   , player :: Thing
                   , location :: Id
                   }

data Action = Action { shouldRun :: World -> Bool
                     , updateWorld :: World -> Maybe World
                     , pre :: [Action]
                     , post :: [Action]
                     , describeAction :: World -> Maybe String
                     }

data Thing = Thing { name :: String
                   , describeThing :: World -> String
                   , stats :: Stats
                   , contents :: [Id]
                   }
