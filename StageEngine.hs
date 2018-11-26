import StageData
import UserInterface

loadGame :: String -> Maybe (World, Map Name [Event])
loadGame = undefined -- Call the compiler to do this


perform :: (Map Name [Event]) -> World -> IO ()
perform events world = do event <- getEventInput currentEvents
                          runResult <- runEvent world event
                          case runResult of
                            Nothing -> gameOver
                            Just world' -> perform world' events
  where currentEvents :: Set Event
        currentEvents = Map.keys (Map.filter (\es -> hasRunnable es) events)
        hasRunnable :: [Event] -> Bool
        hasRunnable = foldr (\e b -> b || (shouldRun e) world) False

-- Describe the world then prompt for an event
getEvent :: (Map Name [Event]) -> World -> IO Event
getEvent events world = do outputWorld world
                           getEventInput 

-- Execute the event and describe what happened
runEvent :: World -> Event -> IO (Maybe World)
runEvent = undefined


main :: IO ()
main = do args <- getArgs
          case args of
          []         -> printUsage
          [gameFile] -> do fileContents <- readFile gameFile
                           game <- loadGame fileContents
                           case g of
                             Nothing              -> printError gameFile
                             Just (world, events) -> perform events world
  where printUsage = putStrLn "Usage: StageEngine gameFile"
        printError filename = putStrLn ("Error: Unable to load game from " ++ filename)


