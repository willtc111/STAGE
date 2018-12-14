module StageData where

import qualified Data.Map.Strict as Map

type Id = String
type Name = String
type Stats = Map.Map Id Integer

data World = World { things :: Map.Map Id Thing
                   , player :: Thing
                   , location :: Id
                   }

data Action = Action { shouldRun :: World -> Bool
                     , updateWorld :: World -> Maybe World
                     , describeAction :: World -> String
                     }

data Thing = Thing { name :: Name
                   , describeThing :: World -> String
                   , stats :: Stats
                   , contents :: [Id]
                   , thingId :: Id
                   , thingClass :: Id
                   }
