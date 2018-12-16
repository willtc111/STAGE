module StageCompilerData where

import StageData
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List as List

data Condition = LocationCondition Pred
               | PlayerCondition Pred
               | OrCondition Condition Condition
               | AndCondition Condition Condition
  deriving (Show, Eq)

data Pred = TruePred
          | IdPred Id
          | ContainsPred Pred
          | ClassPred Id
          | StatPred Id Cmp Expr
          | NotPred Pred
          | OrPred Pred Pred
          | AndPred Pred Pred
  deriving (Show,Eq)

data Cmp = EqCmp
         | NeCmp
         | LtCmp
         | LeCmp
         | GtCmp
         | GeCmp
  deriving (Show,Eq)

data Mod = DoNothingMod
         | SetMod Id Expr
         | GiveMod Id
         | TakeMod Pred
         | IfMod Pred Mod Mod
         | AndMod Mod Mod
  deriving (Show,Eq)

data Expr = NumExpr Integer
          | NegExpr Expr
          | OpExpr Expr Op Expr
          | StatExpr Id
          | ThingStatExpr Id Id
          | PlayerStatExpr Id
  deriving (Show,Eq)

data Op = Add | Sub | Mul | Div | Mod
  deriving (Show,Eq)

data ThingDesc = LiteralTDesc String
               | NameTDesc
               | StatTDesc Id
               | IfPTDesc Pred ThingDesc ThingDesc
               | IfCTDesc Condition ThingDesc ThingDesc
               | ContainedTDesc SubThingDesc String
               | ConcatTDesc [ThingDesc]
  deriving (Show,Eq)

data SubThingDesc = DefaultSubTDesc | CustomSubTDesc ThingDesc
  deriving (Show,Eq)

data ActionDesc = LiteralADesc String
                | IfADesc Condition ActionDesc ActionDesc
                | PlayerADesc SubThingDesc
                | LocationADesc SubThingDesc
                | ConcatADesc [ActionDesc]
  deriving (Show,Eq)

data ClassDecl = ClassDecl
      { classId :: Id
      , classStats :: Stats
      , classDesc :: ThingDesc
      }
  deriving (Show,Eq)

data Class = Class Stats (World -> Thing -> String)

data ThingDecl = ThingDecl
      { thingId :: Id
      , thingClass :: Id
      , name :: Name
      , stats :: Stats
      , contents :: Things
      }
  deriving (Show,Eq)

data ActionDecl = ActionDecl
      { actionName :: Name
      , condition :: Condition
      , modifyPlayer :: Mod
      , modifyCurrentLocation :: Mod
      , newLocation :: Maybe Id
      , actionDesc :: ActionDesc
      }
      | GameEndDecl
      { actionName :: Name
      , condition :: Condition
      , actionDesc :: ActionDesc
      }
  deriving (Show,Eq)

data Decl = ClassDecl' ClassDecl
          | ThingDecl' ThingDecl
          | ActionDecl' ActionDecl
  deriving (Show,Eq)

data Decls = Decls
      { classDecls :: [ClassDecl]
      , thingDecls :: [ThingDecl]
      , actionDecls :: [ActionDecl]
      }
  deriving (Show)

instance Eq Decls where
  (==) a b = (acds List.\\ bcds == []) && (atds List.\\ btds == []) && (aads List.\\ bads == [])
          && (bcds List.\\ acds == []) && (btds List.\\ atds == []) && (bads List.\\ aads == [])
    where
      acds = (classDecls a)
      bcds = (classDecls b)
      atds = (thingDecls a)
      btds = (thingDecls b)
      aads = (actionDecls a)
      bads = (actionDecls b)

emptyDecls :: Decls
emptyDecls = Decls { classDecls = []
                   , thingDecls = []
                   , actionDecls = []
                   }

data PlayerDecl = PlayerDecl
      { playerStats :: Stats
      , playerThings :: Things
      , playerStart :: Id
      , playerDesc :: ThingDesc
      }
  deriving (Show, Eq)

data WorldDescDecl = WorldDescDecl ActionDesc
  deriving (Show, Eq)

data Stage = Stage
      { decls :: [Decl]
      , playerDecl :: PlayerDecl
      , worldDescDecl :: WorldDescDecl
      }
  deriving (Show, Eq)

data StaticData = StaticData
      { classIds :: Set.Set Id
      , thingClasses :: Map.Map Id Id
      }
  deriving (Show)

classExists :: StaticData -> Id -> Bool
classExists sd classId = Set.member classId (classIds sd)

thingExists :: StaticData -> Id -> Bool
thingExists sd thingId = Map.member thingId (thingClasses sd)

thingHasClass :: StaticData -> Id -> Thing -> Bool
thingHasClass sd classId thing =
  Map.lookup (StageData.thingId thing) (thingClasses sd) == Just classId
