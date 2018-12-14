module StagePreprocessor (preprocesssStage) where

import StageLexer
import Text.Parsec
import Data.Functor.Identity

type Parser a = ParsecT String () Identity a

preprocessStage :: String -> String -> Either ParseError String
preprocessStage source = parse pSubstitute source


pSubstitute :: Parser String
pSubstitute = do processed <- choice [pMovingStatement, anyToken]
                 remainder <- pSubstitute
                 return processed ++ remainder

pMovingStatement :: Parser String
pMovingStatement =
  do symbols "Moving from"
     roomFrom <- identifier
     symbol "to"
     roomTo <- identifier
     symbols "is invoked with"
     actionName <- stringLiteral
     symbols "and described by"
     desc <- stringLiteral
     dot
     return "Action \"" ++ actionName ++ "\" is available when location is "
            ++ roomFrom ++ ", modifies player by doing nothing, "
            ++ "modifies current location my doing nothing "
            ++ "before setting location to " ++ roomTo ++ ", "
            ++ "and is described by \"" ++ desc ++ "\"."
