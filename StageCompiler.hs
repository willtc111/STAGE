{-# LANGUAGE RecordWildCards, TupleSections #-}

module StageCompiler (compileStage) where

import Prelude hiding (pred)
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
buildClasses = undefined

buildThings :: Map.Map Id Class -> [ThingDecl] -> Fallible (Map.Map Id Thing)
buildThings = undefined

buildActions :: Set.Set Id -> [ActionDecl] -> Fallible (Map.Map Name [Action])
buildActions = undefined

buildPlayer :: Map.Map Id Thing -> PlayerDecl -> Fallible (Thing, Id)
buildPlayer = undefined

buildCondition :: Condition -> World -> Bool
buildCondition condition = case condition of
  LocationCondition pred -> aux select $ buildPred pred
    where select = \World{..} -> Map.lookup location things
  where aux select test = maybe False test . select

buildPred :: Pred -> Thing -> Bool
buildPred pred = case pred of
  TruePred -> const True
  IdPred s -> (== s) . SD.thingId

buildMod :: Mod -> Thing -> Thing
buildMod = undefined

buildThingDesc :: ThingDesc -> Thing -> World -> String
buildThingDesc = undefined

buildActionDesc :: ActionDesc -> World -> Maybe String
buildActionDesc = undefined
