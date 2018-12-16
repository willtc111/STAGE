module StageData where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type Id = String
type Name = String
type Stats = Map.Map Id Integer
type Things = Set.Set Id

data Game = Game { world :: World
                 , actions :: Map.Map Name [Action]
                 , describeWorld :: World -> String
                 }

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
                   , contents :: Things
                   , thingId :: Id
                   }
