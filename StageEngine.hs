import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe
import System.Environment
import StageData
import qualified UserInterface as UI

loadGame :: String -> IO (Maybe (World, Map.Map Name [Event]))
loadGame = undefined -- Call the compiler to do this


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
     choice <- UI.getEventInput currentEvents
     case choice of
       Nothing -> return Nothing
       Just n  -> return (Just (head (getRunnable (fromJust (Map.lookup n events)))))
  where currentEvents :: Set.Set Name
        currentEvents = Map.keysSet (Map.filter (\es -> hasRunnable es) events)
        getRunnable :: [Event] -> [Event]
        getRunnable es = foldr (\e l -> if (shouldRun e) world then e : l else l ) [] es
        hasRunnable :: [Event] -> Bool
        hasRunnable es = null (getRunnable es)


-- Describe the event and then execute it
runEvent :: World -> Event -> IO (Maybe World)
runEvent world event = do UI.outputEvent world event
                          return ((updateWorld event) world)


main :: IO ()
main = do args <- getArgs
          case args of
            []         -> printUsage
            [gameFile] -> do fileContents <- readFile gameFile
                             game <- loadGame fileContents
                             case game of
                               Nothing              -> printError gameFile
                               Just (world, events) -> perform events world
  where printUsage = putStrLn "Usage: StageEngine gameFile"
        printError filename = putStrLn ("Error: Unable to load game from " ++ filename)


