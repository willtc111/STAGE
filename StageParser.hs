{-# LANGUAGE RecordWildCards, TupleSections #-}

module StageParser (parseStage) where

import StageData
import StageCompilerData
import StageLexer
import qualified Data.Map.Strict as Map
import Text.Parsec
import Text.Parsec.Expr
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

pfChoices :: [Parser a] -> Parser a
pfChoices = choice . map try

pfList :: Parser a -> Parser (a, a, [a])
pfList p = pfChoices [two, many]
  where two = do first <- p
                 symbol "and"
                 second <- p
                 return (first, second, [])
        many = do first <- p
                  comma
                  second <- p
                  comma
                  more <- endBy p comma
                  symbol "and"
                  lastOne <- p
                  return (first, second, more ++ [lastOne])

pfMaybe :: Parser a -> Parser (Maybe a)
pfMaybe = optionMaybe . try

pAn :: Parser ()
pAn = pfChoices [symbols "a", symbols "an"]

pCondition :: Parser Condition
pCondition = pfChoices [ LocationCondition <$> (symbols "the current location" >> pPred)
                       , PlayerCondition <$> (symbols "the player" >> pPred)
                       , do symbol "either"
                            c1 <- pCondition
                            symbol "or"
                            c2 <- pCondition
                            return $ OrCondition c1 c2
                       , do symbol "both"
                            c1 <- pCondition
                            symbol "and"
                            c2 <- pCondition
                            return $ AndCondition c1 c2
                       ]

pPred :: Parser Pred
pPred = pfChoices [ TruePred               <$  symbols "is unconditional"
                  , IdPred                 <$> (symbols "is thing" >> identifier)
                  , NotPred . IdPred       <$> (symbol "is not thing" >> identifier)
                  , ContainsPred           <$> (symbols "does contain something that" >> pPred)
                  , NotPred . ContainsPred <$> (symbols "does not contain something that" >> pPred)
                  , ClassPred              <$> (symbol "is" >> pAn >> identifier)
                  , NotPred . ClassPred    <$> (symbols "is not" >> pAn >> identifier)
                  , StatPred               <$> (symbol "has" >> identifier) <*> pCmp <*> pExpr
                  , OrPred                 <$> (symbol "either" >> pPred) <*> (symbol "or" >> pPred)
                  , AndPred                <$> (symbol "both" >> pPred) <*> (symbol "and" >> pPred)
                  ]

pCmp :: Parser Cmp
pCmp = pfChoices [ EqCmp <$ symbol "="
                 , NeCmp <$ symbol "/="
                 , LtCmp <$ symbol "<"
                 , LeCmp <$ symbol "<="
                 , GtCmp <$ symbol ">"
                 , GeCmp <$ symbol ">="
                 ]

pMod :: Parser Mod
pMod = pfChoices [ const DoNothingMod <$> symbols "doing nothing"
                -- TODO
                 ]

pExpr :: Parser Expr
pExpr = buildExpressionParser table pTerm
  where pTerm = pfChoices [ parens pExpr
                          , NumExpr <$> natural
                          , StatExpr <$> (symbol "its" >> identifier)
                          , do thing <- identifier
                               symbol "'s"
                               stat <- identifier
                               return $ ThingStatExpr thing stat
                          , PlayerStatExpr <$> (symbols "the player's" >> identifier)
                          ]
        table = [ [Prefix $ symbol "-" >> return NegExpr]
                , [op "*" Mul, op "/" Div, op "%" Mod]
                , [op "+" Add, op "-" Sub]
                ]
        op s o = Infix (symbol s >> return (flip OpExpr o)) AssocLeft

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
pThingDecl = do symbol "Thing"
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
                               symbols "modifies the player by"
                               modifyPlayer <- pMod
                               comma
                               symbols "modifies the current location by"
                               modifyCurrentLocation <- pMod
                               newLocation <- pfMaybe (symbols "before setting the current location to" >> identifier)
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
