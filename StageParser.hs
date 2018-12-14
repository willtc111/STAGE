{-# LANGUAGE RecordWildCards, TupleSections #-}

module StageParser (parseStage) where

import StageData
import StageCompilerData
import StageLexer
import qualified Data.Map.Strict as Map
import Text.Parsec
import Data.Functor.Identity
import Data.Bifunctor
import Control.Monad

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

pfChoices :: [Parser a] -> Parser a
pfChoices = choice . map try

pfList :: Parser a -> Parser (a, [a])
pfList p = pfChoices [one, two, many]
  where one = (, []) <$> p
        two = do first <- p
                 symbol "and"
                 second <- p
                 return (first, [second])
        many = do start <- p
                  comma
                  middle <- endBy1 p comma
                  symbol "and"
                  end <- p
                  return $ (start, middle ++ [end])

pfMaybe :: Parser a -> Parser (Maybe a)
pfMaybe = optionMaybe . try

pAn :: Parser ()
pAn = void $ pfChoices [symbol "a", symbol "an"]

pCondition :: Parser Condition
pCondition = pfChoices [ parens pCondition
                       , LocationCondition <$> (symbols "the current location" >> pPred)
                       , PlayerCondition <$> (symbols "the player" >> pPred)
                       , liftM2 OrCondition pCondition pCondition
                       , do (first, rest) <- pfList pCondition
                            return $ foldr AndCondition first rest
                       ]

pPred :: Parser Pred
pPred = pfChoices [ const TruePred <$> symbols "is unconditional"
                  , IdPred         <$> (symbol "is" >> identifier)
                 -- TODO
                  ]

pMod :: Parser Mod
pMod = pfChoices [ const DoNothingMod <$> symbols "doing nothing"
                -- TODO
                 ]

pThingDesc :: Parser ThingDesc
pThingDesc = concatTDesc <$> sepBy1 pThingDescNoConcat (symbol "+")
  where concatTDesc [desc] = desc
        concatTDesc descs  = ConcatTDesc descs
        pThingDescNoConcat =
          pfChoices [ LiteralTDesc  <$> stringLiteral
                    , const NameTDesc <$> symbols "its name"
                   -- TODO
                    ]

pActionDesc :: Parser ActionDesc
pActionDesc = pfChoices [ LiteralADesc <$> stringLiteral
                       -- TODO
                        ]

pClassDecl :: Parser ClassDecl
pClassDecl = do pfChoices [symbol "A", symbol "An"]
                classId <- identifier
                let classStats = Map.empty -- TODO
                symbols "is described by"
                classDesc <- pThingDesc
                return ClassDecl{..}

pThingDecl :: Parser ThingDecl
pThingDecl = do pfChoices [symbol "Thing", symbol "Location"]
                thingId <- identifier
                symbol "is"
                pAn
                thingClass <- identifier
                symbol "named"
                name <- stringLiteral
                let stats = Map.empty -- TODO
                let contents = [] -- TODO
                return ThingDecl{..}

pActionDecl :: Parser ActionDecl
pActionDecl = pfChoices [pNormalActionDecl, pGameEndDecl]
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

pDecl :: Parser Decl
pDecl = do decl <- pfChoices [ ClassDecl' <$> pClassDecl
                             , ThingDecl' <$> pThingDecl
                             , ActionDecl' <$> pActionDecl
                             ]
           dot
           return decl

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

pStage :: Parser (PlayerDecl, [Decl])
pStage = do whiteSpace
            decls1 <- many pDecl
            pPlayerDecl <- pPlayerDecl
            decls2 <- many pDecl
            eof
            return (pPlayerDecl, decls1 ++ decls2)
