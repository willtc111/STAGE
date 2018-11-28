import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.List
import System.Environment
import StageData
import qualified UserInterface as UI

readStage :: String -> Either String (World, Map.Map Name [Action])
readStage = undefined -- Call the compiler to do this


runGame :: (Map.Map Name [Action]) -> World -> IO ()
runGame actions world = do actionChoice <- getAction actions world
                           case actionChoice of
                             Nothing -> gameOver
                             Just action -> do runResult <- runAction world action
                                               case runResult of
                                                 Nothing -> gameOver
                                                 Just world' -> runGame actions world'
  where
    gameOver = return () -- figure out something better to do here

-- Describe the world to the player and get an action choice
getAction :: (Map.Map Name [Action]) -> World -> IO (Maybe Action)
getAction actions world =
  do UI.outputWorld world
     choice <- UI.getActionInput (Map.keysSet availableActions)
     return $ choice >>= flip Map.lookup availableActions
  where availableActions = Map.mapMaybe (find $ flip shouldRun world) actions


-- Describe the action and then execute it
runAction :: World -> Action -> IO (Maybe World)
runAction world action = UI.outputAction world action >> return (updateWorld action world)


main :: IO ()
main = do args <- getArgs
          case args of
            [gameFile] -> do game <- fmap readStage $ readFile gameFile
                             case game of
                               Left  message          -> printError gameFile message
                               Right (world, actions) -> runGame actions world
            _          -> printUsage
  where printUsage = putStrLn "Usage: StageEngine gameFile"
        printError filename message = putStrLn ("Error loading game from " ++ filename ++ ":\n" ++ message)
