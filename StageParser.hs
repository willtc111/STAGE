{-# LANGUAGE RecordWildCards #-}

module StageParser (parseStage) where

import StageData
import StageCompilerData
import StageLexer
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.Parsec
import Text.Parsec.Expr
import Data.Functor.Identity
import Data.Bifunctor
import Data.Maybe

{-
 - For brevity:
 - decl = declaration
 - desc = description
 - pred = predicate
 - mod = modifier
 -}

type Parser a = ParsecT String () Identity a

parseStage :: String -> String -> Either String Stage
parseStage source = bimap show id . parse pStage source

pfChoices :: [Parser a] -> Parser a
pfChoices = choice . map try

pfList :: Parser a -> Parser [a]
pfList p = pfChoices [two, many]
  where two = do first <- p
                 symbol "and"
                 second <- p
                 return [first, second]
        many = do start <- p
                  comma
                  middle <- commaSep1 p
                  comma
                  symbol "and"
                  end <- p
                  return $ start:middle++[end]

pfMaybe :: Parser a -> Parser (Maybe a)
pfMaybe = optionMaybe . try

pfOption :: a -> Parser a -> Parser a
pfOption x = option x . try

pfIfButOtherwise :: (a -> b -> b -> c) -> Parser a -> Parser b -> Parser c
pfIfButOtherwise combine pA pB =
  do symbols "if"
     if' <- pA
     symbol "then"
     then' <- pB
     comma
     symbol "but"
     else' <- pB
     symbol "otherwise"
     return $ combine if' then' else'

pAn :: Parser ()
pAn = lexeme $ char 'a' >> optional (char 'n')

pStats :: Parser Stats
pStats = pfChoices [ uncurry Map.singleton <$> (symbol "stat" >> pStat)
                   , Map.fromList          <$> (symbol "stats" >> pfList pStat)
                   ]
  where pStat = (,) <$> identifier <*> (symbol "=" >> integer)

pThings :: Parser Things
pThings = pfChoices [ Set.singleton <$> (symbol "thing" >> identifier)
                    , Set.fromList  <$> (symbol "things" >> pfList identifier)
                    ]

pCondition :: Parser Condition
pCondition = pfChoices [ LocationCondition <$> (symbols "the current location" >> pPred)
                       , PlayerCondition   <$> (symbols "the player" >> pPred)
                       , OrCondition       <$> (symbol "either" >> pCondition)
                                           <*> (symbol "or" >> pCondition)
                       , AndCondition      <$> (symbol "both" >> pCondition)
                                           <*> (symbol "and" >> pCondition)
                       ]

pPred :: Parser Pred
pPred = pfChoices [ TruePred               <$  symbols "is unconditional"
                  , IdPred                 <$> (symbols "is thing" >> identifier)
                  , NotPred . IdPred       <$> (symbol "is not thing" >> identifier)
                  , ContainsPred           <$> (symbols "does contain something that" >> pPred)
                  , NotPred . ContainsPred <$> (symbols "does not contain something that" >> pPred)
                  , ClassPred              <$> (symbol "is" >> pAn >> identifier)
                  , NotPred . ClassPred    <$> (symbols "is not" >> pAn >> identifier)
                  , StatPred               <$> (symbol "has" >> identifier)
                                           <*> pCmp
                                           <*> pExpr
                  , OrPred                 <$> (symbol "either" >> pPred)
                                           <*> (symbol "or" >> pPred)
                  , AndPred                <$> (symbol "both" >> pPred)
                                           <*> (symbol "and" >> pPred)
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
pMod = pfChoices [ DoNothingMod <$ symbols "doing nothing"
                 , SetMod       <$> (symbols "setting its" >> identifier)
                                <*> (symbol "to" >> pExpr)
                 , GiveMod      <$> (symbols "giving it" >> identifier)
                 , TakeMod      <$> (symbols "taking away everything it contains that" >> pPred)
                 , pfIfButOtherwise IfMod (symbol "it" >> pPred) pMod
                 , AndMod       <$> (symbol "first" >> pMod)
                                <*> (symbols "and then" >> pMod)
                 ]

pExpr :: Parser Expr
pExpr = buildExpressionParser table pTerm
  where pTerm = pfChoices [ parens pExpr
                          , NumExpr        <$> natural
                          , StatExpr       <$> (symbol "its" >> identifier)
                          , ThingStatExpr  <$> identifier
                                           <*> (symbol "'s" >> identifier)
                          , PlayerStatExpr <$> (symbols "the player's" >> identifier)
                          ]
        table = [ [Prefix $ symbol "-" >> return NegExpr]
                , [op "*" Mul, op "/" Div, op "%" Mod]
                , [op "+" Add, op "-" Sub]
                ]
        op s o = Infix (symbol s >> return (flip OpExpr o)) AssocLeft

pThingDesc :: Parser ThingDesc
pThingDesc = ConcatTDesc <$> sepBy1 pThingDescNoConcat (symbol "+")
  where pThingDescNoConcat =
          pfChoices [ LiteralTDesc   <$> stringLiteral
                    , NameTDesc      <$  symbols "its name"
                    , StatTDesc      <$> (symbols "the value of its" >> identifier)
                    , pfIfButOtherwise IfPTDesc (symbol "it" >> pPred) pThingDesc
                    , pfIfButOtherwise IfCTDesc pCondition pThingDesc
                    , ContainedTDesc <$> (symbols "for each contained thing," >> pSubThingDesc)
                                     <*> (comma >> symbols "separated by String" >> stringLiteral)
                    ]

pSubThingDesc :: Parser SubThingDesc
pSubThingDesc = pfChoices [ DefaultSubTDesc <$  symbols "its description"
                          , CustomSubTDesc  <$> pThingDesc
                          ]

pActionDesc :: Parser ActionDesc
pActionDesc = ConcatADesc <$> sepBy1 pActionDescNoConcat (symbol "+")
  where pActionDescNoConcat =
          pfChoices [ LiteralADesc  <$> stringLiteral
                    , pfIfButOtherwise IfADesc pCondition pActionDesc
                    , PlayerADesc   <$> between (symbols "description by") (symbols "of the player") pSubThingDesc
                    , LocationADesc <$> between (symbols "description by") (symbols "of the current location") pSubThingDesc
                    ]

pClassDecl :: Parser ClassDecl
pClassDecl = do lexeme $ char 'A' >> optional (char 'n')
                classId <- identifier
                classStats <- pfOption Map.empty $ between (symbol "has") (symbol "and") pStats
                symbols "is described by"
                classDesc <- pThingDesc
                return ClassDecl{..}

pThingDecl :: Parser ThingDecl
pThingDecl = do symbol "Thing"
                thingId <- identifier
                symbol "is"
                pAn
                thingClass <- identifier
                name <- pfOption "" $ symbol "named" >> stringLiteral
                stats <- pfOption Map.empty $ symbol "with" >> pStats
                contents <- pfOption Set.empty $ symbols "that contains" >> pThings
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
                               newLocation <- pfMaybe $ symbols "before setting the current location to" >> identifier
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
pDecl = do decl <- pfChoices [ ClassDecl'  <$> pClassDecl
                             , ThingDecl'  <$> pThingDecl
                             , ActionDecl' <$> pActionDecl
                             ]
           dot
           return decl

pPlayerDecl :: Parser PlayerDecl
pPlayerDecl = do symbols "The player"
                 playerStats <- pfOption Map.empty $ between (symbol "has") (symbol "and") pStats
                 playerThings <- pfOption Set.empty $ between (symbol "has") (symbol "and") pThings
                 playerDesc <- pfOption (LiteralTDesc "") $ between (symbols "is described by") (symbol "and") pThingDesc
                 symbols "starts in"
                 playerStart <- identifier
                 dot
                 return PlayerDecl{..}

pWorldDescDecl :: Parser WorldDescDecl
pWorldDescDecl = WorldDescDecl <$> between (symbols "The game state is described by") dot pActionDesc

pStage :: Parser Stage
pStage = do whiteSpace
            declsBefore <- many pDecl
            Stage{..} <- pfChoices [pPlayerFirst, pGameStateFirst]
            declsAfter <- many pDecl
            eof
            return Stage{decls = declsBefore ++ decls ++ declsAfter, ..}
  where pPlayerFirst    = do playerDecl <- pPlayerDecl
                             decls <- many pDecl
                             worldDescDecl <- pWorldDescDecl
                             return Stage{..}
        pGameStateFirst = do worldDescDecl <- pWorldDescDecl
                             decls <- many pDecl
                             playerDecl <- pPlayerDecl
                             return Stage{..}
