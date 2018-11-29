{-# LANGUAGE RecordWildCards #-}

module StageCompiler (compileStage) where

import StageParser
import StageData
import StageCompilerData
import qualified Data.Map.Strict as Map

compileStage :: String -> IO (Either String (World, Map.Map Name [Action]))
compileStage source = do parseResult <- parseStage source <$> readFile source
                         return $ parseResult >>= build

build :: (PlayerDecl, [Decl]) -> Either String (World, Map.Map Name [Action])
build (playerDecl, decls) = undefined
  where Decls{..} = sortDecls decls

sortDecls :: [Decl] -> Decls
sortDecls = foldr aux emptyDecls
  where aux (ThingDescDecl'  decl) decls = decls{thingDescDecls  = decl:(thingDescDecls  decls)}
        aux (ActionDescDecl' decl) decls = decls{actionDescDecls = decl:(actionDescDecls decls)}
        aux (ClassDecl'      decl) decls = decls{classDecls      = decl:(classDecls      decls)}
        aux (ThingDecl'      decl) decls = decls{thingDecls      = decl:(thingDecls      decls)}
        aux (ActionDecl'     decl) decls = decls{actionDecls     = decl:(actionDecls     decls)}
