module StageCompilerData where

import StageData

data Condition = LocationCondition Pred
              -- TODO
  deriving (Show)

data Pred = TruePred
          | IdPred String
         -- TODO
  deriving (Show)

data Mod = DoNothingMod
        -- TODO
  deriving (Show)

data ThingDesc = LiteralTDesc String
               | NameTDesc
               | ConcatTDesc [ThingDesc]
              -- TODO
  deriving (Show)

data ActionDesc = LiteralADesc String
               -- TODO
  deriving (Show)

data ClassDecl = ClassDecl
      { classId :: Id
      , classStats :: Stats
      , classDesc :: ThingDesc
      }
  deriving (Show)

data Class = Class Stats (Thing -> World -> String)

data ThingDecl = ThingDecl
      { thingId :: Id
      , thingClass :: Id
      , name :: Name
      , stats :: Stats
      , contents :: [Id]
      }
  deriving (Show)

data ActionDecl = ActionDecl
      { actionName :: Name
      , condition :: Condition
      , modifyPlayer :: Mod
      , modifyCurrentLocation :: Mod
      , newLocation :: Id
      , actionDesc :: ActionDesc
      }
      | GameEndDecl
      { actionName :: Name
      , condition :: Condition
      , actionDesc :: ActionDesc
      }
  deriving (Show)

data Decl = ClassDecl' ClassDecl
          | ThingDecl' ThingDecl
          | ActionDecl' ActionDecl
  deriving (Show)

data Decls = Decls { classDecls :: [ClassDecl]
                   , thingDecls :: [ThingDecl]
                   , actionDecls :: [ActionDecl]
                   }
  deriving (Show)

emptyDecls :: Decls
emptyDecls = Decls { classDecls = []
                   , thingDecls = []
                   , actionDecls = []
                   }

data PlayerDecl = PlayerDecl
                    { playerStats :: Stats
                    , playerThings :: [Id]
                    , playerStart :: Id
                    , playerDesc :: ThingDesc
                    }
  deriving (Show)
