module StageLexer where

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language

lexer = P.makeTokenParser haskellStyle{P.identLetter = alphaNum <|> char '_'}

identifier = P.identifier lexer
stringLiteral = P.stringLiteral lexer
natural = P.natural lexer
integer = P.integer lexer
symbol = P.symbol lexer
symbols = mapM_ symbol . words
lexeme = P.lexeme lexer
whiteSpace = P.whiteSpace lexer
parens = P.parens lexer
comma = P.comma lexer
commaSep1 = P.commaSep1 lexer
dot = P.dot lexer
