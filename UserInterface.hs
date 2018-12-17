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
  do putStr "> "
     input <- getLine
     case input of
       command
         | Set.member command actions  -> return $ Just command
         | "help" `isPrefixOf` command -> displayActionOptions >> getActionInput actions
         | otherwise                   -> printTryAgainMessage >> getActionInput actions
  where
    displayActionOptions = do putStrLn "Available actions:"
                              putStr (foldr (\name s -> name ++ "\n" ++ s) "\n" (Set.elems actions))
    printTryAgainMessage = do putStrLn "Unrecognized action. Please try again or enter \"help\" to show options."
