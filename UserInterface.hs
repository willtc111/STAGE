module UserInterface (outputGame, outputAction, getActionInput) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.List

import StageData


outputGame :: Game -> IO ()
outputGame game = putStrLn $ (describeWorld game) (world game)


outputAction :: World -> Action -> IO ()
outputAction world action = putStrLn (describeAction action world)


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
