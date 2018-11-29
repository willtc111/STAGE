module StageLexer where

import qualified Text.Parsec.Token as P
import Text.Parsec.Language

lexer = P.makeTokenParser haskellStyle

identifier = P.identifier lexer
stringLiteral = P.stringLiteral lexer
integer = P.integer lexer
symbol = P.symbol lexer
lexeme = P.lexeme lexer
whiteSpace = P.whiteSpace lexer
parens = P.parens lexer
comma = P.comma lexer
commaSep1 = P.commaSep1 lexer
dot = P.dot lexer
