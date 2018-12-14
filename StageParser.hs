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

pfSymbols :: String -> Parser ()
pfSymbols = mapM_ symbol . words

pfChoices :: [Parser a] -> Parser a
pfChoices = choice . map try

pfList :: Parser a -> Parser [a]
pfList p = pfChoices [one, two, many]
  where one = (:[]) <$> p
        two = do first <- p
                 symbol "and"
                 second <- p
                 return [first, second]
        many = do start <- p
                  comma
                  middle <- endBy1 p comma
                  symbol "and"
                  end <- p
                  return $ start : middle ++ [end]

pfMaybe :: Parser a -> Parser (Maybe a)
pfMaybe = optionMaybe . try

pAn :: Parser ()
pAn = pfChoices [symbol "a", symbol "an"] >> return ()

pCondition :: Parser Condition
pCondition = pfChoices [ LocationCondition <$> (symbol "location" >> pPred)
                      -- TODO
                       ]

pPred :: Parser Pred
pPred = pfChoices [ const TruePred <$> pfSymbols "is unconditional"
                  , IdPred         <$> (symbol "is" >> identifier)
                 -- TODO
                  ]

pMod :: Parser Mod
pMod = pfChoices [ const DoNothingMod <$> pfSymbols "doing nothing"
                -- TODO
                 ]

pThingDesc :: Parser ThingDesc
pThingDesc = concatTDesc <$> sepBy1 pThingDescNoConcat (symbol "+")
  where concatTDesc [desc] = desc
        concatTDesc descs  = ConcatTDesc descs
        pThingDescNoConcat =
          pfChoices [ LiteralTDesc  <$> stringLiteral
                    , const NameTDesc <$> pfSymbols "its name"
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
                pfSymbols "is described by"
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
                               pfSymbols "is available when"
                               condition <- pCondition
                               comma
                               pfSymbols "modifies player by"
                               modifyPlayer <- pMod
                               comma
                               pfSymbols "modifies current location by"
                               modifyCurrentLocation <- pMod
                               pfSymbols "before setting location to"
                               newLocation <- identifier
                               comma
                               pfSymbols "and is described by"
                               actionDesc <- pActionDesc
                               return ActionDecl{..}
        pGameEndDecl = do symbol "Action"
                          actionName <- stringLiteral
                          pfSymbols "is available when"
                          condition <- pCondition
                          comma
                          pfSymbols "ends the game"
                          comma
                          pfSymbols "and is described by"
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
pPlayerDecl = do pfSymbols "The player"
                 let playerStats = Map.empty -- TODO
                 let playerThings = [] -- TODO
                 pfSymbols "starts in"
                 playerStart <- identifier
                 pfSymbols "and is described by"
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
