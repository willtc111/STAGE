import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.List
import System.Environment
import StageData
import qualified UserInterface as UI

readGame :: String -> Either String (World, Map.Map Name [Event])
readGame = undefined -- Call the compiler to do this


perform :: (Map.Map Name [Event]) -> World -> IO ()
perform events world = do eventChoice <- getEvent events world
                          case eventChoice of
                            Nothing -> gameOver
                            Just event -> do runResult <- runEvent world event
                                             case runResult of
                                               Nothing -> gameOver
                                               Just world' -> perform events world'
  where
    gameOver = return () -- figure out something better to do here

-- Describe the world to the player and get an event choice
getEvent :: (Map.Map Name [Event]) -> World -> IO (Maybe Event)
getEvent events world =
  do UI.outputWorld world
     choice <- UI.getEventInput (Map.keysSet availableEvents)
     return $ choice >>= flip Map.lookup availableEvents
  where availableEvents = Map.mapMaybe (find $ flip shouldRun world) events


-- Describe the event and then execute it
runEvent :: World -> Event -> IO (Maybe World)
runEvent world event = UI.outputEvent world event >> return (updateWorld event world)


main :: IO ()
main = do args <- getArgs
          case args of
            [gameFile] -> do game <- fmap readGame $ readFile gameFile
                             case game of
                               Left  message         -> printError gameFile message
                               Right (world, events) -> perform events world
            _          -> printUsage
  where printUsage = putStrLn "Usage: StageEngine gameFile"
        printError filename message = putStrLn ("Error loading game from " ++ filename ++ ":\n" ++ message)
