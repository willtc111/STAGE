{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE RecordWildCards, TupleSections #-}

module StageCompiler (compileStage) where

import Prelude hiding (pred, mod)
import qualified Prelude as Math (mod)
import StagePreprocessor
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
compileStage source = do sourceContents <- readFile source
                         return $ do preprocessed <- preprocessStage source sourceContents
                                     parsed <- parseStage source preprocessed
                                     buildStage parsed

buildStage :: (PlayerDecl, [Decl]) -> Fallible (World, Map.Map Name [Action])
buildStage (playerDecl, decls) =
  do let d@Decls{..} = sortDecls decls
         staticData = buildStaticData d
     classes <- buildClasses staticData classDecls
     things <- buildThings classes thingDecls
     actions <- buildActions staticData actionDecls
     (player, location) <- buildPlayer staticData playerDecl
     return (World{..}, actions)

sortDecls :: [Decl] -> Decls
sortDecls = foldr aux emptyDecls
  where aux (ClassDecl'      decl) decls = decls{classDecls      = decl:(classDecls      decls)}
        aux (ThingDecl'      decl) decls = decls{thingDecls      = decl:(thingDecls      decls)}
        aux (ActionDecl'     decl) decls = decls{actionDecls     = decl:(actionDecls     decls)}

buildStaticData :: Decls -> StaticData
buildStaticData Decls{..} = StaticData{..}
  where classIds = Set.fromList $ map classId classDecls
        thingClasses = Map.fromList $ map (liftM2 (,) thingId thingClass) thingDecls

buildMapFromDeclsWith :: Ord k => (Map.Map k v -> a -> Fallible (k, v)) -> [a] -> Fallible (Map.Map k v)
buildMapFromDeclsWith f = foldr ((=<<) . aux) (return Map.empty)
  where aux a m = do (k, v) <- f m a
                     return $ Map.insert k v m

buildClasses :: StaticData -> [ClassDecl] -> Fallible (Map.Map Id Class)
buildClasses staticData = buildMapFromDeclsWith $
  \classes ClassDecl{..} ->
    if isNothing (Map.lookup classId classes)
      then (classId,) <$> Class classStats <$> (buildThingDesc staticData classDesc)
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

buildActions :: StaticData -> [ActionDecl] -> Fallible (Map.Map Name [Action])
buildActions staticData = buildMapFromDeclsWith $
  \actions decl -> case decl of
    ActionDecl{..} ->
      do case newLocation of
           Nothing  -> return ()
           Just loc -> unless (thingExists staticData loc)
                         (fail $ "Unrecognized thing id: " ++ loc)
         shouldRun <- buildCondition staticData condition
         modifyPlayer' <- buildMod staticData modifyPlayer
         modifyCurrentLocation' <- buildMod staticData modifyCurrentLocation
         let updateWorld w@World{..} =
               do currentLocation <- Map.lookup location things
                  let location' = fromMaybe location newLocation
                  return World { things = Map.insert location (modifyCurrentLocation' w currentLocation) things
                               , player = modifyPlayer' w player
                               , location = location'
                               }
         describeAction <- buildActionDesc staticData actionDesc
         let bucket = fromMaybe [] $ Map.lookup actionName actions
         return (actionName, Action{..}:bucket)
    GameEndDecl{..} -> undefined

buildPlayer :: StaticData -> PlayerDecl -> Fallible (Thing, Id)
buildPlayer staticData PlayerDecl{..} =
  do let validateId thingId = unless (thingExists staticData thingId)
                                (fail $ "Unrecognized thing id: " ++ thingId)
     mapM_ validateId $ playerStart:playerThings
     desc <- buildThingDesc staticData playerDesc
     let player = Thing { name = ""
                        , describeThing = desc player
                        , stats = playerStats
                        , contents = playerThings
                        , thingId = ""
                        }
     return (player, playerStart)


buildThingDesc :: StaticData -> ThingDesc -> Fallible (Thing -> World -> String)
buildThingDesc staticData desc = case desc of
  LiteralTDesc s    -> return (\_ _ -> s)
  NameTDesc         -> return (\Thing{..} _ -> name)
  ConcatTDesc descs -> foldr aux (return $ \_ _ -> "") $ map (buildThingDesc staticData) descs
                        where aux d1 d2 =
                                do d1' <- d1
                                   d2' <- d2
                                   return $ \thing world -> d1' thing world ++ d2' thing world

buildActionDesc :: StaticData -> ActionDesc -> Fallible (World -> String)
buildActionDesc staticData desc = case desc of
  LiteralADesc s -> return $ const s

buildCondition :: StaticData -> Condition -> Fallible (World -> Bool)
buildCondition staticData condition = case condition of
  LocationCondition pred ->
    do pred' <- buildPred staticData pred
       let getLocation World{..} = Map.lookup location things
           test world = maybe False (pred' world)
       return $ \world -> test world $ getLocation world
  PlayerCondition pred ->
    do pred' <- buildPred staticData pred
       return $ \world -> pred' world $ player world
  OrCondition c1 c2 -> combine (||) c1 c2
  AndCondition c1 c2 -> combine (&&) c1 c2
  where combine f c1 c2 = do c1' <- buildCondition staticData c1
                             c2' <- buildCondition staticData c2
                             return $ liftM2 f c1' c2'

buildPred :: StaticData -> Pred -> Fallible (World -> Thing -> Bool)
buildPred staticData pred = case pred of
  TruePred -> return $ \_ _ -> True
  IdPred thingId ->
    if thingExists staticData thingId
      then return $ \_ t -> SD.thingId t == thingId
      else fail $ "Unrecognized thing id: " ++ thingId
  ContainsPred p ->
    do p' <- buildPred staticData p
       return $ \w t -> any (maybe False (p' w) . flip Map.lookup (things w)) (SD.contents t)
  ClassPred thingClass -> return $ \_ t -> thingHasClass staticData thingClass t
  StatPred stat cmp expr ->
    do let cmp' = buildCmp cmp
       expr' <- buildExpr staticData expr
       return $ \w t -> cmp' (getStat stat t) (expr' w t)
  NotPred p ->
    do p' <- buildPred staticData p
       return $ \w t -> not $ p' w t
  OrPred p1 p2 ->
    do p1' <- buildPred staticData p1
       p2' <- buildPred staticData p2
       return $ \w t -> (p1' w t) || (p2' w t)
  AndPred p1 p2 ->
    do p1' <- buildPred staticData p1
       p2' <- buildPred staticData p2
       return $ \w t -> (p1' w t) && (p2' w t)

buildCmp :: Cmp -> (Integer -> Integer -> Bool)
buildCmp EqCmp = (==)
buildCmp NeCmp = (/=)
buildCmp LtCmp = (<)
buildCmp LeCmp = (<=)
buildCmp GtCmp = (>)
buildCmp GeCmp = (>=)

buildMod :: StaticData -> Mod -> Fallible (World -> Thing -> Thing)
buildMod staticData mod = case mod of
  DoNothingMod -> return $ \_ t -> t

buildExpr :: StaticData -> Expr -> Fallible (World -> Thing -> Integer)
buildExpr staticData expr = case expr of
  NumExpr x -> return $ \_ _ -> x
  NegExpr e ->
    do e' <- buildExpr staticData e
       return $ \w t -> negate $ e' w t
  OpExpr e1 o e2 ->
    do e1' <- buildExpr staticData e1
       e2' <- buildExpr staticData e2
       let o' = buildOp o
       return $ \w t -> o' (e1' w t) (e2' w t)

buildOp :: Op -> (Integer -> Integer -> Integer)
buildOp Add = (+)
buildOp Sub = (-)
buildOp Mul = (*)
buildOp Div = div
buildOp Mod = Math.mod

getStat :: Id -> Thing -> Integer
getStat stat = Map.findWithDefault 0 stat . SD.stats
