{-# LANGUAGE RecordWildCards, TupleSections #-}

module StageCompiler (compileStage) where

import Prelude hiding (pred, mod)
import StageParser
import StageData hiding (thingId)
import qualified StageData as SD
import StageCompilerData
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type Fallible = Either String

compileStage :: String -> IO (Fallible (World, Map.Map Name [Action]))
compileStage source = do parseResult <- parseStage source <$> readFile source
                         return $ parseResult >>= buildStage

buildStage :: (PlayerDecl, [Decl]) -> Fallible (World, Map.Map Name [Action])
buildStage (playerDecl, decls) =
  do let Decls{..} = sortDecls decls
     classes <- buildClasses classDecls
     things <- buildThings classes thingDecls
     actions <- buildActions (Map.keysSet things) actionDecls
     (player, location) <- buildPlayer things playerDecl
     return (World{..}, actions)

sortDecls :: [Decl] -> Decls
sortDecls = foldr aux emptyDecls
  where aux (ClassDecl'      decl) decls = decls{classDecls      = decl:(classDecls      decls)}
        aux (ThingDecl'      decl) decls = decls{thingDecls      = decl:(thingDecls      decls)}
        aux (ActionDecl'     decl) decls = decls{actionDecls     = decl:(actionDecls     decls)}

buildClasses :: [ClassDecl] -> Fallible (Map.Map Id Class)
buildClasses = foldr ((=<<) . aux) (return Map.empty)
  where aux ClassDecl{..} classes =
          case Map.lookup classId classes of
            Nothing -> return $ Map.insert classId (Class classStats (buildThingDesc classDesc)) classes
            Just _  -> fail   $ "Duplicate class id: " ++ classId

buildThings :: Map.Map Id Class -> [ThingDecl] -> Fallible (Map.Map Id Thing)
buildThings = undefined

buildActions :: Set.Set Id -> [ActionDecl] -> Fallible (Map.Map Name [Action])
buildActions = undefined

buildPlayer :: Map.Map Id Thing -> PlayerDecl -> Fallible (Thing, Id)
buildPlayer = undefined

buildCondition :: Condition -> World -> Bool
buildCondition (LocationCondition pred) = test . select
  where select World{..} = Map.lookup location things
        test = maybe False $ buildPred pred

buildPred :: Pred -> Thing -> Bool
buildPred TruePred   = const True
buildPred (IdPred s) = (== s) . SD.thingId

buildMod :: Mod -> Thing -> Thing
buildMod DoNothingMod = id

buildThingDesc :: ThingDesc -> Thing -> World -> String
buildThingDesc (LiteralTDesc s)    = \_ _ -> s
buildThingDesc IdTDesc             = \Thing{..} _ -> thingId
buildThingDesc (ConcatTDesc descs) = foldr aux (\_ _ -> "") $ map buildThingDesc descs
  where aux d1 d2 = \thing world -> d1 thing world ++ d2 thing world

buildActionDesc :: ActionDesc -> World -> String
buildActionDesc (LiteralADesc s) = const s
