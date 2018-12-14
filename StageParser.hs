{-# LANGUAGE RecordWildCards #-}

module StageParser (parseStage) where

import StageData
import StageCompilerData
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

parseStage :: String -> String -> Either String (PlayerDecl, [Decl])
parseStage source = bimap show id . parse pStage source

symbols :: String -> Parser ()
symbols = mapM_ symbol . words

eitherSymbol :: String -> String -> Parser String
eitherSymbol s1 s2 = try (symbol s1) <|> symbol s2

tryChoices :: [Parser a] -> Parser a
tryChoices = choice . map try

pStage :: Parser (PlayerDecl, [Decl])
pStage = do whiteSpace
            decls1 <- many pDecl
            pPlayerDecl <- pPlayerDecl
            decls2 <- many pDecl
            eof
            return (pPlayerDecl, decls1 ++ decls2)

pDecl :: Parser Decl
pDecl = do decl <- tryChoices [ ClassDecl' <$> pClassDecl
                              , ThingDecl' <$> pThingDecl
                              , ActionDecl' <$> pActionDecl
                              ]
           dot
           return decl

pClassDecl :: Parser ClassDecl
pClassDecl = do eitherSymbol "A" "An"
                classId <- identifier
                let classStats = Map.empty -- TODO
                symbols "is described by"
                classDesc <- pThingDesc
                return ClassDecl{..}

pThingDecl :: Parser ThingDecl
pThingDecl = do symbol "Thing"
                thingId <- identifier
                symbol "is"
                eitherSymbol "a" "an"
                thingClass <- identifier
                symbol "named"
                name <- stringLiteral
                let stats = Map.empty -- TODO
                let contents = [] -- TODO
                return ThingDecl{..}

pActionDecl :: Parser ActionDecl
pActionDecl = tryChoices [pNormalActionDecl, pGameEndDecl]
  where pNormalActionDecl = do symbol "Action"
                               actionName <- stringLiteral
                               symbols "is available when"
                               condition <- pCondition
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
        pGameEndDecl = do symbol "Action"
                          actionName <- stringLiteral
                          symbols "is available when"
                          condition <- pCondition
                          comma
                          symbols "ends the game"
                          comma
                          symbols "and is described by"
                          actionDesc <- pActionDesc
                          return GameEndDecl{..}

pPlayerDecl :: Parser PlayerDecl
pPlayerDecl = do symbols "The player"
                 let playerStats = Map.empty -- TODO
                 let playerThings = [] -- TODO
                 symbols "starts in"
                 playerStart <- identifier
                 symbols "and is described by"
                 playerDesc <- pThingDesc
                 dot
                 return PlayerDecl{..}

pThingDesc :: Parser ThingDesc
pThingDesc = concatTDesc <$> sepBy1 pThingDescNoConcat (symbol "+")
  where concatTDesc [desc] = desc
        concatTDesc descs  = ConcatTDesc descs
        pThingDescNoConcat =
          tryChoices [ LiteralTDesc  <$> stringLiteral
                     , const NameTDesc <$> symbols "its name"
                    -- TODO
                     ]

pActionDesc :: Parser ActionDesc
pActionDesc = tryChoices [ LiteralADesc <$> stringLiteral
                        -- TODO
                         ]

pCondition :: Parser Condition
pCondition = tryChoices [ LocationCondition <$> (symbol "location" >> pPred)
                       -- TODO
                        ]

pMod :: Parser Mod
pMod = tryChoices [ const DoNothingMod <$> symbols "doing nothing"
                 -- TODO
                  ]

pPred :: Parser Pred
pPred = tryChoices [ const TruePred <$> symbols "is unconditional"
                   , IdPred         <$> (symbol "is" >> identifier)
                  -- TODO
                   ]
