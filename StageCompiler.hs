{-# LANGUAGE RecordWildCards, TupleSections #-}

module StageCompiler (compileStage) where

import Prelude hiding (pred, mod)
import StageParser
import StageData hiding (thingId)
import qualified StageData as SD
import StageCompilerData
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe
import Control.Monad

type Fallible = Either String

compileStage :: String -> IO (Fallible (World, Map.Map Name [Action]))
compileStage source = do parseResult <- parseStage source <$> readFile source
                         return $ parseResult >>= buildStage

buildStage :: (PlayerDecl, [Decl]) -> Fallible (World, Map.Map Name [Action])
buildStage (playerDecl, decls) =
  do let Decls{..} = sortDecls decls
         thingIds = Set.fromList $ map thingId thingDecls
     classes <- buildClasses thingIds classDecls
     things <- buildThings classes thingDecls
     actions <- buildActions thingIds actionDecls
     (player, location) <- buildPlayer thingIds playerDecl
     return (World{..}, actions)

sortDecls :: [Decl] -> Decls
sortDecls = foldr aux emptyDecls
  where aux (ClassDecl'      decl) decls = decls{classDecls      = decl:(classDecls      decls)}
        aux (ThingDecl'      decl) decls = decls{thingDecls      = decl:(thingDecls      decls)}
        aux (ActionDecl'     decl) decls = decls{actionDecls     = decl:(actionDecls     decls)}

buildMapFromDeclsWith :: Ord k => (Map.Map k v -> a -> Fallible (k, v)) -> [a] -> Fallible (Map.Map k v)
buildMapFromDeclsWith f = foldr ((=<<) . aux) (return Map.empty)
  where aux a m = do (k, v) <- f m a
                     return $ Map.insert k v m

buildClasses :: Set.Set Id -> [ClassDecl] -> Fallible (Map.Map Id Class)
buildClasses thingIds = buildMapFromDeclsWith $
  \classes ClassDecl{..} ->
    if isNothing (Map.lookup classId classes)
      then (classId,) <$> Class classStats <$> (buildThingDesc thingIds classDesc)
      else fail $ "Duplicate class id: " ++ classId

buildThings :: Map.Map Id Class -> [ThingDecl] -> Fallible (Map.Map Id Thing)
buildThings classes = buildMapFromDeclsWith $
  \things ThingDecl{..} ->
    do when (isJust $ Map.lookup thingId things)
         (fail $ "Duplicate thing id: " ++ thingId)
       Class classStats desc <-
         maybe (fail $ "Unrecognized class id: " ++ thingClass)
               return
               (Map.lookup thingClass classes)
       let stats = Map.union stats classStats
           describeThing = desc thing
           thing = Thing{..}
       return (thingId, thing)

buildActions :: Set.Set Id -> [ActionDecl] -> Fallible (Map.Map Name [Action])
buildActions thingIds = buildMapFromDeclsWith $
  \actions decl -> case decl of
    ActionDecl{..} ->
      do unless (Set.member newLocation thingIds)
           (fail $ "Unrecognized thing id: " ++ newLocation)
         shouldRun <- buildCondition thingIds condition
         modifyPlayer' <- buildMod thingIds modifyPlayer
         modifyCurrentLocation' <- buildMod thingIds modifyCurrentLocation
         let updateWorld World{..} =
               do currentLocation <- Map.lookup location things
                  return World { things = Map.insert location (modifyCurrentLocation' currentLocation) things
                               , player = modifyPlayer' player
                               , location = newLocation
                               }
         describeAction <- buildActionDesc thingIds actionDesc
         let bucket = fromMaybe [] $ Map.lookup actionName actions
         return (actionName, Action{..}:bucket)
    GameEndDecl{..} -> undefined

buildPlayer :: Set.Set Id -> PlayerDecl -> Fallible (Thing, Id)
buildPlayer thingIds PlayerDecl{..} =
  do let validateId thingId = unless (Set.member thingId thingIds)
                                (fail $ "Unrecognized thing id: " ++ playerStart)
     mapM_ validateId $ playerStart:playerThings
     desc <- buildThingDesc thingIds playerDesc
     let player = Thing { name = ""
                        , describeThing = desc player
                        , stats = playerStats
                        , contents = playerThings
                        , thingId = ""
                        , thingClass = ""
                        }
     return (player, playerStart)


buildThingDesc :: Set.Set Id -> ThingDesc -> Fallible (Thing -> World -> String)
buildThingDesc thingIds desc = case desc of
  LiteralTDesc s    -> return (\_ _ -> s)
  NameTDesc         -> return (\Thing{..} _ -> name)
  ConcatTDesc descs -> foldr aux (return $ \_ _ -> "") $ map (buildThingDesc thingIds) descs
                        where aux d1 d2 =
                                do d1' <- d1
                                   d2' <- d2
                                   return $ \thing world -> d1' thing world ++ d2' thing world

buildActionDesc :: Set.Set Id -> ActionDesc -> Fallible (World -> String)
buildActionDesc thingIds desc = case desc of
  LiteralADesc s -> return $ const s

buildCondition :: Set.Set Id -> Condition -> Fallible (World -> Bool)
buildCondition thingIds condition = case condition of
  LocationCondition pred ->
    do pred' <- buildPred thingIds pred
       let select World{..} = Map.lookup location things
           test = maybe False pred'
       return $ test . select

buildPred :: Set.Set Id -> Pred -> Fallible (Thing -> Bool)
buildPred thingIds pred = case pred of
  TruePred       -> return $ const True
  IdPred thingId -> if Set.member thingId thingIds
                      then return $ (== thingId) . SD.thingId
                      else fail $ "Unrecognized thing id: " ++ thingId

buildMod :: Set.Set Id -> Mod -> Fallible (Thing -> Thing)
buildMod thingIds mod = case mod of
  DoNothingMod -> return id
