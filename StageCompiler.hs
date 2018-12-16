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
import Data.List
import Control.Monad

type Fallible = Either String

compileStage :: String -> IO (Fallible Game)
compileStage source = do sourceContents <- readFile source
                         return $ do preprocessed <- preprocessStage source sourceContents
                                     parsed <- parseStage source preprocessed
                                     buildStage parsed

buildStage :: Stage -> Fallible Game
buildStage Stage{..} =
  do let d@Decls{..} = sortDecls decls
         staticData = buildStaticData d
     classes <- buildClasses staticData classDecls
     things <- buildThings classes thingDecls
     actions <- buildActions staticData actionDecls
     (player, location) <- buildPlayer staticData playerDecl
     describeWorld <- buildWorldDesc staticData worldDescDecl
     return Game{world = World{..}, ..}

sortDecls :: [Decl] -> Decls
sortDecls = foldr aux emptyDecls
  where aux (ClassDecl'      decl) decls = decls{classDecls      = decl:classDecls      decls}
        aux (ThingDecl'      decl) decls = decls{thingDecls      = decl:thingDecls      decls}
        aux (ActionDecl'     decl) decls = decls{actionDecls     = decl:actionDecls     decls}

buildStaticData :: Decls -> StaticData
buildStaticData Decls{..} = StaticData{..}
  where classIds = Set.fromList $ map classId classDecls
        thingClasses = Map.fromList $ map (liftM2 (,) thingId thingClass) thingDecls

validateThing :: StaticData -> Id -> Fallible ()
validateThing staticData thingId =
  unless (thingExists staticData thingId) $ fail $ "Unrecognized thing id: " ++ thingId

validateActionName :: Name -> Fallible ()
validateActionName = let reserved = flip Set.notMember reservedNames
                     in \name -> when (reserved name) $ fail $ "Reserved action name: " ++ name
  where reservedNames = Set.fromList [ "help"
                                     , "quit"
                                     ]

buildMapFromDeclsWith :: Ord k => (Map.Map k v -> a -> Fallible (k, v)) -> [a] -> Fallible (Map.Map k v)
buildMapFromDeclsWith f = foldr ((=<<) . aux) (return Map.empty)
  where aux a m = do (k, v) <- f m a
                     return $ Map.insert k v m

buildClasses :: StaticData -> [ClassDecl] -> Fallible (Map.Map Id Class)
buildClasses staticData = buildMapFromDeclsWith $
  \classes ClassDecl{..} ->
    if isNothing (Map.lookup classId classes)
      then (classId,) . Class classStats <$> buildThingDesc staticData classDesc
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
           describeThing = flip desc thing
           thing = Thing{..}
       return (thingId, thing)

buildActions :: StaticData -> [ActionDecl] -> Fallible (Map.Map Name [Action])
buildActions staticData = buildMapFromDeclsWith $
  \actions decl ->
    let addAction name action = let bucket = fromMaybe [] $ Map.lookup name actions
                                in return (name, action:bucket)
    in case decl of
      ActionDecl{..} ->
        do maybe (return ()) (validateThing staticData) newLocation
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
           addAction actionName Action{..}
      GameEndDecl{..} ->
        do shouldRun <- buildCondition staticData condition
           describeAction <- buildActionDesc staticData actionDesc
           let updateWorld = const Nothing
           addAction actionName Action{..}


buildPlayer :: StaticData -> PlayerDecl -> Fallible (Thing, Id)
buildPlayer staticData PlayerDecl{..} =
  do validateThing staticData playerStart
     mapM_ (validateThing staticData) playerThings
     desc <- buildThingDesc staticData playerDesc
     let player = Thing { name = ""
                        , describeThing = flip desc player
                        , stats = playerStats
                        , contents = playerThings
                        , thingId = ""
                        }
     return (player, playerStart)

buildWorldDesc :: StaticData -> WorldDescDecl -> Fallible (World -> String)
buildWorldDesc staticData (WorldDescDecl desc) = buildActionDesc staticData desc

buildThingDesc :: StaticData -> ThingDesc -> Fallible (World -> Thing -> String)
buildThingDesc staticData desc = case desc of
  LiteralTDesc s -> return $ \_ _ -> s
  NameTDesc      -> return $ \_ Thing{..} -> name
  StatTDesc stat -> return $ \_ t -> show $ getStat stat t
  IfPTDesc pred d1 d2 ->
    do pred' <- buildPred staticData pred
       d1' <- buildThingDesc staticData d1
       d2' <- buildThingDesc staticData d2
       return $ \w t -> if pred' w t then d1' w t else d2' w t
  IfCTDesc cond d1 d2 ->
    do cond' <- buildCondition staticData cond
       d1' <- buildThingDesc staticData d1
       d2' <- buildThingDesc staticData d2
       return $ \w t -> if cond' w then d1' w t else d2' w t
  ContainedTDesc subDesc s ->
    do subDesc' <- buildSubThingDesc staticData subDesc
       return $ \w t -> let aux = fmap (subDesc' w) . flip Map.lookup (things w)
                        in intercalate s $ mapMaybe aux $ Set.toList $ SD.contents t
  ConcatTDesc descs ->
    foldr (aux . buildThingDesc staticData) (return $ \_ _ -> "") descs
      where aux d1 d2 = do d1' <- d1
                           d2' <- d2
                           return $ \t w -> d1' t w ++ d2' t w

buildSubThingDesc :: StaticData -> SubThingDesc -> Fallible (World -> Thing -> String)
buildSubThingDesc _          DefaultSubTDesc        = return $ flip describeThing
buildSubThingDesc staticData (CustomSubTDesc tDesc) = buildThingDesc staticData tDesc

buildActionDesc :: StaticData -> ActionDesc -> Fallible (World -> String)
buildActionDesc staticData desc = case desc of
  LiteralADesc s -> return $ const s
  IfADesc cond d1 d2 ->
    do cond' <- buildCondition staticData cond
       d1' <- buildActionDesc staticData d1
       d2' <- buildActionDesc staticData d2
       return $ \w -> if cond' w then d1' w else d2' w
  PlayerADesc desc ->
    do desc' <- buildSubThingDesc staticData desc
       return $ \w -> desc' w (player w)
  LocationADesc desc ->
    do desc' <- buildSubThingDesc staticData desc
       return $ \w -> let currentLocation = Map.lookup (location w) (SD.things w)
                      in maybe "" (desc' w) currentLocation
  ConcatADesc descs ->
    foldr (aux . buildActionDesc staticData) (return $ const "") descs
      where aux d1 d2 = do d1' <- d1
                           d2' <- d2
                           return $ \w -> d1' w ++ d2' w

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
    do validateThing staticData thingId
       return $ \_ t -> SD.thingId t == thingId
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
       return $ \w t -> p1' w t || p2' w t
  AndPred p1 p2 ->
    do p1' <- buildPred staticData p1
       p2' <- buildPred staticData p2
       return $ \w t -> p1' w t && p2' w t

buildCmp :: Cmp -> (Integer -> Integer -> Bool)
buildCmp EqCmp = (==)
buildCmp NeCmp = (/=)
buildCmp LtCmp = (<)
buildCmp LeCmp = (<=)
buildCmp GtCmp = (>)
buildCmp GeCmp = (>=)

buildMod :: StaticData -> Mod -> Fallible (World -> Thing -> Thing)
buildMod staticData mod = case mod of
  DoNothingMod -> return $ const id
  SetMod stat expr ->
    do expr' <- buildExpr staticData expr
       return $ \w t -> setStat stat (expr' w t) t
  GiveMod thingId ->
    do validateThing staticData thingId
       return $ \_ Thing{..} -> Thing{contents = Set.insert thingId contents, ..}
  TakeMod pred ->
    do pred' <- buildPred staticData pred
       let p w = maybe False (pred' w) . flip Map.lookup (SD.things w)
       return $ \w Thing{..} -> Thing{contents = Set.filter (p w) contents, ..}
  IfMod pred thenMod elseMod ->
    do pred' <- buildPred staticData pred
       thenMod' <- buildMod staticData thenMod
       elseMod' <- buildMod staticData elseMod
       return $ \w t -> if pred' w t then thenMod' w t else elseMod' w t
  AndMod m1 m2 ->
    do m1' <- buildMod staticData m1
       m2' <- buildMod staticData m2
       return $ liftM2 (.) m2' m1'

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
  StatExpr stat -> return $ \_ t -> getStat stat t
  ThingStatExpr thingId stat ->
    do validateThing staticData thingId
       return $ \w _ -> getStat stat $ fromJust $ Map.lookup thingId (SD.things w)
  PlayerStatExpr stat -> return $ \w _ -> getStat stat (SD.player w)

buildOp :: Op -> (Integer -> Integer -> Integer)
buildOp Add = (+)
buildOp Sub = (-)
buildOp Mul = (*)
buildOp Div = div
buildOp Mod = Math.mod

getStat :: Id -> Thing -> Integer
getStat stat = Map.findWithDefault 0 stat . SD.stats

setStat :: Id -> Integer -> Thing -> Thing
setStat stat val Thing{..} = Thing{stats = Map.insert stat val stats, ..}
