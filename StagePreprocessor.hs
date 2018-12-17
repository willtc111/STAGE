module StagePreprocessor (preprocessStage) where

import StageLexer
import Text.Parsec
import Text.Parsec.Char
import Data.Functor.Identity
import Data.Bifunctor

type Macro = ParsecT String () Identity String

preprocessStage :: String -> String -> Either String String
preprocessStage source = bimap show id . parse mStage source


mStage :: Macro
mStage = do macroExpansions <- many $ choice $ map try macros
            eof
            return $ concat macroExpansions
  where macros = [ mAlwaysAvailable
                 , mLocation
                 , mMovingDecl
                 , mTakeDropDecl
                 , mQuitDecl
                 , (:[]) <$> anyChar
                 ]

mAlwaysAvailable :: Macro
mAlwaysAvailable = symbols "is always available" >> return "is available when the player is unconditional"

mLocation :: Macro
mLocation = do symbol "Location"
               loc <- identifier
               symbol "is"
               return $ "Thing " ++ loc ++ " is "

mMovingDecl :: Macro
mMovingDecl =
  do symbols "Moving from"
     place1 <- identifier
     symbol "to"
     place2 <- identifier
     symbols "is invoked with"
     name <- stringLiteral
     symbols "and described by"
     desc <- stringLiteral
     dot
     return $ concat [ "Action "
                     , show name
                     , " is available when the current location is thing "
                     , place1
                     , ", modifies the player by doing nothing, "
                     , "modifies the current location by doing nothing "
                     , "before setting the current location to "
                     , place2
                     , ", and is described by "
                     , show desc
                     , ".\n"
                     ]

mTakeDropDecl :: Macro
mTakeDropDecl =
  do symbols "The player can take and drop"
     thing <- identifier
     symbol "as"
     name <- stringLiteral
     dot
     return $ concat [ "Action "
                     , show ("take " ++ name)
                     , " is available when the current location does contain"
                     , " something that is thing "
                     , thing
                     , ", modifies the player by giving it "
                     , thing
                     , ", modifies the current location by taking away"
                     , " everything it contains that is thing "
                     , thing
                     , ", and is described by "
                     , show ("You take " ++ name ++ ".")
                     , ".\n"
                     , "Action "
                     , show ("drop " ++ name)
                     , " is available when the player does contain"
                     , " something that is thing "
                     , thing
                     , ", modifies the player by taking away"
                     , " everything it contains that is thing "
                     , thing
                     , ", modifies the current location by giving it "
                     , thing
                     , ", and is described by "
                     , show ("You drop " ++ name ++ ".")
                     , ".\n"
                     ]

mQuitDecl :: Macro
mQuitDecl = symbols "The default quit action is available."
          >> return "Action \"quit\" is available when the player is unconditional, ends the game, and is described by \"\"."
