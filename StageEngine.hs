{-# LANGUAGE RecordWildCards #-}

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.List
import System.Environment
import StageData
import StageCompiler
import qualified UserInterface as UI

runGame :: Game -> IO ()
runGame game = do actionChoice <- getAction game
                  case actionChoice of
                       Nothing -> gameOver
                       Just action -> do runResult <- runAction (world game) action
                                         case runResult of
                                           Nothing -> gameOver
                                           Just world' -> runGame game{world = world'}
  where
    gameOver = return () -- figure out something better to do here

-- Describe the world to the player and get an action choice
getAction :: Game -> IO (Maybe Action)
getAction game@Game{..} =
  do UI.outputGame game
     choice <- UI.getActionInput (Map.keysSet availableActions)
     return $ choice >>= flip Map.lookup availableActions
  where availableActions = Map.mapMaybe (find $ flip shouldRun world) actions


-- Describe the action and then execute it
runAction :: World -> Action -> IO (Maybe World)
runAction world action = UI.outputAction world action >> return (updateWorld action world)


main :: IO ()
main = do args <- getArgs
          case args of
            [gameFile] -> do game <- compileStage gameFile
                             either (printError gameFile) runGame game
            _          -> printUsage
  where printUsage = putStrLn "Usage: StageEngine gameFile"
        printError filename message = putStrLn ("Error loading game from " ++ filename ++ ":\n" ++ message)
