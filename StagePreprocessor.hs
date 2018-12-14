module StagePreprocessor (preprocessStage) where

import StageLexer
import Text.Parsec
import Text.Parsec.Char
import Data.Functor.Identity
import Data.Bifunctor

type Parser a = ParsecT String () Identity a

preprocessStage :: String -> String -> Either String String
preprocessStage source = bimap show id . parse pMacroExpand source


pMacroExpand :: Parser String
pMacroExpand = do macroExpansions <- many $ choice $ map try macros
                  eof
                  return $ concat macroExpansions
  where macros = [ pMovingDecl
                 , pAlwaysAvailable
                 , pLocation
                 , (:[]) <$> anyChar
                 ]

pMovingDecl :: Parser String
pMovingDecl =
  do symbols "Moving from"
     roomFrom <- identifier
     symbol "to"
     roomTo <- identifier
     symbols "is invoked with"
     actionName <- stringLiteral
     symbols "and described by"
     desc <- stringLiteral
     string "."
     return $ concat [ "Action \""
                     , actionName
                     , "\" is available when the current location is "
                     , roomFrom
                     , ", modifies the player by doing nothing, "
                     , "modifies the current location by doing nothing "
                     , "before setting the current location to "
                     , roomTo
                     , ", "
                     , "and is described by \""
                     , desc
                     , "\"."
                     ]

pAlwaysAvailable :: Parser String
pAlwaysAvailable = symbols "is always available" >> return "is available when the player is unconditional"

pLocation :: Parser String
pLocation = do symbol "Location"
               loc <- identifier
               symbol "is"
               return $ "Thing " ++ loc ++ " is "
