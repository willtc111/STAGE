module UserInterface (outputWorld, outputThing, outputAction, getActionInput) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.List

import StageData


outputWorld :: World -> IO ()
outputWorld world = do outputThing world currentLocation
                       outputThing world (player world)
  where currentLocation = fromJust (Map.lookup (location world) (things world))


outputThing :: World -> Thing -> IO ()
outputThing world thing = putStrLn (describeThing thing world)


outputAction :: World -> Action -> IO ()
outputAction world action = case (describeAction action world) of
                              Nothing -> return ()
                              Just description -> putStrLn description


getActionInput :: (Set.Set Name) -> IO (Maybe Name)
getActionInput actions =
  do putStrLn "What do you want to do next?"
     input <- getLine
     case input of
       command
         | Set.member command actions  -> return $ Just command
         | "quit" `isPrefixOf` command -> return Nothing
         | "help" `isPrefixOf` command -> displayActionOptions >> getActionInput actions
         | otherwise                   -> printTryAgainMessage >> getActionInput actions
  where
    displayActionOptions = do putStrLn "help"
                              putStrLn "quit"
                              putStr (foldr (\name s -> name ++ "\n" ++ s) "\n" (Set.elems actions))
    printTryAgainMessage = do putStrLn "Unrecognized command, please try again or enter:"
                              putStrLn "\"help\" to show options"
                              putStrLn "\"quit\" to exit the game"
