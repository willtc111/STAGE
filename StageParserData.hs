module StageParserData where

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
               | IdTDesc
               | ConcatTDesc [ThingDesc]
              -- TODO
  deriving (Show)

data ActionDesc = LiteralADesc String
               -- TODO
  deriving (Show)

data ThingDescDecl = ThingDescDecl
      { thingDescId :: Id
      , describeThing :: ThingDesc
      }
  deriving (Show)

data ActionDescDecl = ActionDescDecl
      { actionDescId :: Id
      , describeAction :: ActionDesc
      }
  deriving (Show)

data ClassDecl = ClassDecl
      { classId :: Id
      , parents :: [Id]
      , statNames :: [Id]
      , classDesc :: ThingDesc
      }
  deriving (Show)

data ThingDecl = ThingDecl
      { thingId :: Id
      , thingClass :: Id
      , stats :: Stats
      , containing :: [Id]
      }
  deriving (Show)

data ActionDecl = ActionDecl
      { actionName :: Name
      , shouldRun :: Condition
      , modifyPlayer :: Mod
      , modifyCurrentLocation :: Mod
      , newLocation :: Id
      , actionDesc :: ActionDesc
      }
  deriving (Show)

data Decl = ThingDescDecl' ThingDescDecl
          | ActionDescDecl' ActionDescDecl
          | ClassDecl' ClassDecl
          | ThingDecl' ThingDecl
          | ActionDecl' ActionDecl

data Decls = Decls { thingDescDecls :: [ThingDescDecl]
                   , actionDescDecls :: [ActionDescDecl]
                   , classDecls :: [ClassDecl]
                   , thingDecls :: [ThingDecl]
                   , actionDecls :: [ActionDecl]
                   }

emptyDecls :: Decls
emptyDecls = Decls { thingDescDecls = []
                   , actionDescDecls = []
                   , classDecls = []
                   , thingDecls = []
                   , actionDecls = []
                   }

data PlayerDecl = PlayerDecl
                    { playerStats :: Stats
                    , playerThings :: [Id]
                    , playerDesc :: ThingDesc
                    }
  deriving (Show)
