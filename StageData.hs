module StageData where

import qualified Data.Map.Strict as Map

type Id = String
type Name = String
type Stats = Map.Map String Integer

data World = World { things :: Map.Map Id Thing
                   , player :: Thing
                   , location :: Id
                   }

data Action = Action { shouldRun :: World -> Bool
                     , updateWorld :: World -> Maybe World
                     , pre :: [Action] -- pre/post are for a future extension
                     , post :: [Action] -- they are not currently used for anything
                     , describeAction :: World -> String
                     }

data Thing = Thing { name :: Name
                   , describeThing :: World -> String
                   , stats :: Stats
                   , contents :: [Id]
                   , thingId :: Id -- used in predicates
                   , thingClass :: Id -- used in predicates
                   }
