{-# LANGUAGE RecordWildCards #-}

module StageParser (readStage) where

import StageData
import StageParserData
import StageLexer
import qualified Data.Map.Strict as Map
import Text.Parsec
import Data.Functor.Identity
import Data.Bifunctor

{-
 - For brevity:
 - decl = declaration
 - desc = description
 - pred = predicate
 - mod = modifier
 -}

type Parser a = ParsecT String () Identity a

readStage :: String -> Either String (World, Map.Map Name [Action])
readStage s = (bimap show id $ parse pStage "" s) >>= build

-- build

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



-- parse

symbols :: String -> Parser ()
symbols = mapM_ symbol . words

eitherSymbol :: String -> String -> Parser String
eitherSymbol s1 s2 = try (symbol s1) <|> symbol s2

tryChoice :: [Parser a] -> Parser a
tryChoice = choice . map try

pStage :: Parser (PlayerDecl, [Decl])
pStage = do whiteSpace
            decls1 <- many pDecl
            pPlayerDecl <- pPlayerDecl
            decls2 <- many pDecl
            eof
            return (pPlayerDecl, decls1 ++ decls2)

pDecl :: Parser Decl
pDecl = do decl <- tryChoice [ ThingDescDecl' <$> pThingDescDecl
                             , ActionDescDecl' <$> pActionDescDecl
                             , ClassDecl' <$> pClassDecl
                             , ThingDecl' <$> pThingDecl
                             , ActionDecl' <$> pActionDecl
                             ]
           dot
           return decl

pThingDescDecl :: Parser ThingDescDecl
pThingDescDecl = do symbol "Description"
                    thingDescId <- identifier
                    symbols "describes a thing by"
                    describeThing <- pThingDesc
                    return ThingDescDecl{..}

pActionDescDecl :: Parser ActionDescDecl
pActionDescDecl = do symbol "Description"
                     actionDescId <- identifier
                     symbols "describes an action by"
                     describeAction <- pActionDesc
                     return ActionDescDecl{..}

pClassDecl :: Parser ClassDecl
pClassDecl = do eitherSymbol "A" "An"
                classId <- identifier
                let parents = [] -- TODO
                let statNames = [] -- TODO
                symbols "is described by"
                classDesc <- pThingDesc
                return ClassDecl{..}

pThingDecl :: Parser ThingDecl
pThingDecl = do symbol "Thing"
                thingId <- identifier
                symbol "is"
                eitherSymbol "a" "an"
                thingClass <- identifier
                let stats = Map.empty -- TODO
                let containing = [] -- TODO
                return ThingDecl{..}

pActionDecl :: Parser ActionDecl
pActionDecl = do symbol "Action"
                 actionName <- stringLiteral
                 symbols "is available when"
                 shouldRun <- pCondition
                 comma
                 symbols "modifies player by"
                 modifyPlayer <- pMod
                 comma
                 symbols "modifies current location by"
                 modifyCurrentLocation <- pMod
                 symbols "before setting location to"
                 newLocation <- identifier
                 comma
                 symbols "and is described by"
                 actionDesc <- pActionDesc
                 return ActionDecl{..}

pPlayerDecl :: Parser PlayerDecl
pPlayerDecl = do symbols "The player"
                 let playerStats = Map.empty -- TODO
                 let playerThings = [] -- TODO
                 symbols "is described by"
                 playerDesc <- pThingDesc
                 dot
                 return PlayerDecl{..}

pThingDesc :: Parser ThingDesc
pThingDesc = concatTDesc <$> sepBy1 pThingDescNoConcat (symbol "+")
  where concatTDesc [desc] = desc
        concatTDesc descs  = ConcatTDesc descs
        pThingDescNoConcat =
          tryChoice [ LiteralTDesc  <$> stringLiteral
                    , const IdTDesc <$> symbols "its id"
                   -- TODO
                    ]

pActionDesc :: Parser ActionDesc
pActionDesc = tryChoice [ LiteralADesc <$> stringLiteral
                       -- TODO
                        ]

pCondition :: Parser Condition
pCondition = tryChoice [ LocationCondition <$> (symbol "location" >> pPred)
                      -- TODO
                       ]

pMod :: Parser Mod
pMod = tryChoice [ const DoNothingMod <$> symbols "doing nothing"
                -- TODO
                 ]

pPred :: Parser Pred
pPred = tryChoice [ const TruePred <$> symbols "is unconditional"
                  , IdPred         <$> (symbol "is" >> identifier)
                 -- TODO
                  ]
