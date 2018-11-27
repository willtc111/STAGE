module UserInterface (outputWorld, outputThing, outputEvent, getEventInput) where

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


outputEvent :: World -> Event -> IO ()
outputEvent world event = case (describeEvent event world) of
                            Nothing -> return ()
                            Just description -> putStrLn description


getEventInput :: (Set.Set Name) -> IO (Maybe Name)
getEventInput events =
  do putStrLn "What do you want to do next?"
     input <- getLine
     case input of
       command
         | Set.member command events   -> return $ Just command
         | "quit" `isPrefixOf` command -> return Nothing
         | "help" `isPrefixOf` command -> displayEventOptions >> getEventInput events
         | otherwise                   -> printTryAgainMessage >> getEventInput events
  where
    displayEventOptions = do putStrLn "help"
                             putStrLn "quit"
                             putStr (foldr (\name s -> name ++ "\n" ++ s) "\n" (Set.elems events))
    printTryAgainMessage = do putStrLn "Unrecognized command, please try again or enter:"
                              putStrLn "\"help\" to show options"
                              putStrLn "\"quit\" to exit the game"
