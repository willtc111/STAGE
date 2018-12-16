{-# LANGUAGE RecordWildCards, FlexibleInstances, TypeSynonymInstances #-}

module StageParserTests where

import StageParser
import qualified StageData as SD
import qualified StageCompilerData as SCD

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Text.PrettyPrint
import Test.QuickCheck
import Test.QuickCheck.Instances
import Control.Monad

{- Tests -}
checkParseStage :: SCD.Stage -> Bool
checkParseStage stage =
  case (parseStage "" (render $ stageD stage)) of
    Left  _ -> False
    Right s -> stage == s


{- Generators -}

alphaFrequencies :: [(Int, Gen Char)]
alphaFrequencies = [(26, choose ('a', 'z')),
                    (26, choose ('A', 'Z')),
                    (1,  return '_')]

digitFrequencies :: [(Int, Gen Char)]
digitFrequencies = [(10, choose ('0', '9'))]

genAlpha :: Gen Char
genAlpha = frequency alphaFrequencies

genAlphaNum :: Gen Char
genAlphaNum = frequency $ alphaFrequencies ++ digitFrequencies

genAlphaNumString :: Gen String
genAlphaNumString = listOf1 genAlphaNum

genId :: Gen SD.Id
genId = genAlphaNumString

genName :: Gen SD.Name
genName = genAlphaNumString

genStats :: Gen (Map.Map SD.Id Integer)
genStats = Map.fromList <$> genStatList
  where
    genStatList :: Gen [(SD.Id, Integer)]
    genStatList = listOf genStat
    genStat :: Gen (SD.Id, Integer)
    genStat = liftM2 (,) genId (arbitrary :: Gen Integer)

genThings :: Gen SD.Things
genThings = Set.fromList <$> listOf genId


genCondition :: Gen SCD.Condition
genCondition = oneof [(liftM SCD.LocationCondition genPred),
                      (liftM SCD.PlayerCondition genPred),
                      (liftM2 SCD.OrCondition genCondition genCondition),
                      (liftM2 SCD.AndCondition genCondition genCondition)]

instance Arbitrary SCD.Condition where
  arbitrary = genCondition


genPred :: Gen SCD.Pred
genPred = oneof [(return SCD.TruePred),
                 (liftM SCD.IdPred genId),
                 (liftM SCD.ContainsPred genPred),
                 (liftM SCD.ClassPred genId),
                 (liftM3 SCD.StatPred genId genCmp genExpr),
                 (liftM SCD.NotPred (oneof [(liftM SCD.IdPred genId),
                                            (liftM SCD.ContainsPred genPred),
                                            (liftM SCD.ClassPred genId)])),
                 (liftM2 SCD.OrPred genPred genPred),
                 (liftM2 SCD.AndPred genPred genPred)]

instance Arbitrary SCD.Pred where
  arbitrary = genPred


genCmp :: Gen SCD.Cmp
genCmp = oneof [(return SCD.EqCmp),
                (return SCD.NeCmp),
                (return SCD.LtCmp),
                (return SCD.LeCmp),
                (return SCD.GtCmp),
                (return SCD.GeCmp)]

instance Arbitrary SCD.Cmp where
  arbitrary = genCmp


genMod :: Gen SCD.Mod
genMod = oneof [(return SCD.DoNothingMod),
                (liftM2 SCD.SetMod genId genExpr),
                (liftM SCD.GiveMod genId),
                (liftM SCD.TakeMod genPred),
                (liftM3 SCD.IfMod genPred genMod genMod),
                (liftM2 SCD.AndMod genMod genMod)]

instance Arbitrary SCD.Mod where
  arbitrary = genMod


genExpr :: Gen SCD.Expr
genExpr = oneof [(liftM SCD.NumExpr (arbitrary :: Gen Integer)),
                 (liftM SCD.NegExpr genExpr),
                 (liftM3 SCD.OpExpr genExpr genOp genExpr),
                 (liftM SCD.StatExpr genId),
                 (liftM2 SCD.ThingStatExpr genId genId),
                 (liftM SCD.PlayerStatExpr genId)]

instance Arbitrary SCD.Expr where
  arbitrary = genExpr


genOp :: Gen SCD.Op
genOp = oneof [(return SCD.Add),
               (return SCD.Sub),
               (return SCD.Mul),
               (return SCD.Div),
               (return SCD.Mod)]

instance Arbitrary SCD.Op where
  arbitrary = genOp


genThingDesc :: Gen SCD.ThingDesc
genThingDesc = frequency [(25, liftM SCD.LiteralTDesc genAlphaNumString),
                          (25, return SCD.NameTDesc),
                          (25, liftM SCD.StatTDesc genId),
                          (5,  liftM3 SCD.IfPTDesc genPred genThingDesc genThingDesc),
                          (5,  liftM3 SCD.IfCTDesc genCondition genThingDesc genThingDesc),
                          (14, liftM2 SCD.ContainedTDesc genSubThingDesc genAlphaNumString),
                          (1,  liftM SCD.ConcatTDesc (listOf genThingDesc))]

instance Arbitrary SCD.ThingDesc where
  arbitrary = genThingDesc


genSubThingDesc :: Gen SCD.SubThingDesc
genSubThingDesc = oneof [(return SCD.DefaultSubTDesc),
                         (liftM SCD.CustomSubTDesc genThingDesc)]

instance Arbitrary SCD.SubThingDesc where
  arbitrary = genSubThingDesc


genActionDesc :: Gen SCD.ActionDesc
genActionDesc = frequency [(49, liftM SCD.LiteralADesc genAlphaNumString),
                           (10, liftM3 SCD.IfADesc genCondition genActionDesc genActionDesc),
                           (20, liftM SCD.PlayerADesc genSubThingDesc),
                           (20, liftM SCD.LocationADesc genSubThingDesc),
                           (1,  liftM SCD.ConcatADesc (listOf genActionDesc))]

instance Arbitrary SCD.ActionDesc where
  arbitrary = genActionDesc


genClassDecl :: Gen SCD.ClassDecl
genClassDecl = do classId <- genId
                  classStats <- genStats
                  classDesc <- genThingDesc
                  return SCD.ClassDecl{..}

instance Arbitrary SCD.ClassDecl where
  arbitrary = genClassDecl


genThingDecl :: Gen SCD.ThingDecl
genThingDecl = do thingId <- genId
                  thingClass <- genId
                  name <- genName
                  stats <- genStats
                  contents <- genThings
                  return SCD.ThingDecl{..}

instance Arbitrary SCD.ThingDecl where
  arbitrary = genThingDecl


genActionDecl :: Gen SCD.ActionDecl
genActionDecl = oneof [
  (do actionName <- genName
      condition <- genCondition
      modifyPlayer <- genMod
      modifyCurrentLocation <- genMod
      newLocation <- genMaybeId
      actionDesc <- genActionDesc
      return SCD.ActionDecl{..} ),
  (do actionName <- genName
      condition <- genCondition
      actionDesc <- genActionDesc
      return SCD.GameEndDecl{..})]
  where
    genMaybeId :: Gen (Maybe SD.Id)
    genMaybeId = oneof [(Just <$> genId), (return Nothing)]

instance Arbitrary SCD.ActionDecl where
  arbitrary = genActionDecl


genDecl :: Gen SCD.Decl
genDecl = oneof [(liftM SCD.ClassDecl' genClassDecl),
                 (liftM SCD.ThingDecl' genThingDecl),
                 (liftM SCD.ActionDecl' genActionDecl)]

instance Arbitrary SCD.Decl where
  arbitrary = genDecl


genDecls :: Gen SCD.Decls
genDecls = do classDecls <- listOf genClassDecl
              thingDecls <- listOf genThingDecl
              actionDecls <- listOf genActionDecl
              return SCD.Decls{..}

instance Arbitrary SCD.Decls where
  arbitrary = genDecls


genPlayerDecl :: Gen SCD.PlayerDecl
genPlayerDecl = do playerStats <- genStats
                   playerThings <- genThings
                   playerStart <- genId
                   playerDesc <- genThingDesc
                   return SCD.PlayerDecl{..}

instance Arbitrary SCD.PlayerDecl where
  arbitrary = genPlayerDecl


genWorldDescDecl :: Gen SCD.WorldDescDecl
genWorldDescDecl = liftM SCD.WorldDescDecl genActionDesc

instance Arbitrary SCD.WorldDescDecl where
  arbitrary = genWorldDescDecl


genStage :: Gen SCD.Stage
genStage = do decls <- listOf genDecl
              playerDecl <- genPlayerDecl
              worldDescDecl <- genWorldDescDecl
              return SCD.Stage{..}

instance Arbitrary SCD.Stage where
  arbitrary = genStage


{- Pretty printers -}

idD :: SD.Id -> Doc
idD id = text id

strLitD :: String -> Doc
strLitD str = text $ "\"" ++ str ++ "\""

nameD :: SD.Name -> Doc
nameD name = strLitD name

statD :: (SD.Id, Integer) -> Doc
statD (id, val) = idD id <+> text "=" <+> integer val

statsD :: SD.Stats -> Doc
statsD stats = aux (Map.toList stats)
  where
    aux [stat]   = text "stat" <+> statD stat
    aux statList = text "stats" <+> listD statD statList

listD :: (a -> Doc) -> [a] -> Doc
listD d [elem]  = d elem
listD d [e1,e2] = d e1 <+> text "and" <+> d e2
listD d elems   = aux elems
  where
    aux [e]    = text "and" <+> d e
    aux (e:es) = d e <> text "," <+> aux es

conditionD :: SCD.Condition -> Doc
conditionD (SCD.LocationCondition pred)   = text "the player"
                                            <+> predD pred
conditionD (SCD.PlayerCondition pred)     = text "the current location"
                                            <+> predD pred
conditionD (SCD.OrCondition cond1 cond2)  = text "either"
                                            <+> conditionD cond1
                                            <+> text "or"
                                            <+> conditionD cond2
conditionD (SCD.AndCondition cond1 cond2) = text "both"
                                            <+> conditionD cond1
                                            <+> text "and"
                                            <+> conditionD cond2

predD :: SCD.Pred -> Doc
predD p = case p of
            (SCD.NotPred p')-> aux False p'
            _           -> aux True p
  where
    aux :: Bool -> SCD.Pred -> Doc
    aux _ (SCD.TruePred)             = text "is unconditional"
    aux n (SCD.IdPred id)            = text "is"
                                       <+> (if n then empty else text "not")
                                       <+> text "thing"
                                       <+> idD id
    aux n (SCD.ContainsPred pred)    = text "does"
                                       <+> (if n then empty else text "not")
                                       <+> text "contain something that"
                                       <+> predD pred
    aux n (SCD.ClassPred id)         = text "is"
                                       <+> (if n then empty else text "not")
                                       <+> text "a"
                                       <+> idD id
    aux n (SCD.StatPred id cmp expr) = text "has"
                                       <+> idD id
                                       <+> cmpD cmp
                                       <+> exprD expr
    aux n (SCD.OrPred pred1 pred2)   = text "either"
                                       <+> predD pred1
                                       <+> text "or"
                                       <+> predD pred2
    aux n (SCD.AndPred pred1 pred2)  = text "both"
                                       <+> predD pred1
                                       <+> text "and"
                                       <+> predD pred2


cmpD :: SCD.Cmp -> Doc
cmpD SCD.EqCmp = text "="
cmpD SCD.NeCmp = text "/="
cmpD SCD.LtCmp = text "<"
cmpD SCD.LeCmp = text "<="
cmpD SCD.GtCmp = text ">"
cmpD SCD.GeCmp = text ">="


modD :: SCD.Mod -> Doc
modD (SCD.DoNothingMod)         = text "doing nothing"
modD (SCD.SetMod id expr)       = text "setting its"
                                  <+> idD id
                                  <+> text "to"
                                  <+> exprD expr
modD (SCD.GiveMod id)           = text "giving it"
                                  <+> idD id
modD (SCD.TakeMod pred)         = text "taking away everything it contains that"
                                  <+> predD pred
modD (SCD.IfMod pred modT modF) = text "if it"
                                  <+> predD pred
                                  <+> text "then"
                                  <+> modD modT
                                  <> text ", but"
                                  <+> modD modF
                                  <+> text "otherwise"
modD (SCD.AndMod mod1 mod2)     = text "first"
                                  <+> modD mod1
                                  <+> text "and then"
                                  <+> modD mod2

exprD :: SCD.Expr -> Doc
exprD (SCD.NumExpr int) = integer int
exprD (SCD.NegExpr expr) = text "-" <> exprD expr
exprD (SCD.OpExpr expr1 op expr2) = exprD expr1 <+> opD op <+> exprD expr2
exprD (SCD.StatExpr id) = text "its" <+> idD id
exprD (SCD.ThingStatExpr id1 id2) = idD id1 <> text "'s" <+> idD id2
exprD (SCD.PlayerStatExpr id) = text "the player's" <+> idD id

opD :: SCD.Op -> Doc
opD (SCD.Add) = text "+"
opD (SCD.Sub) = text "-"
opD (SCD.Mul) = text "*"
opD (SCD.Div) = text "/"
opD (SCD.Mod) = text "%"

thingDescD :: SCD.ThingDesc -> Doc
thingDescD (SCD.LiteralTDesc str) =
  strLitD str
thingDescD (SCD.NameTDesc) =
  text "its name"
thingDescD (SCD.StatTDesc id) =
  text "the value of its"
  <+> idD id
thingDescD (SCD.IfPTDesc pred tDescT tDescF) =
  text "if it"
  <+> predD pred
  <+> text "then"
  <+> thingDescD tDescT
  <> text ", but"
  <+> thingDescD tDescF
  <+> text "otherwise"
thingDescD (SCD.IfCTDesc cond tDescT tDescF) =
  text "if"
  <+> conditionD cond
  <+> text "then"
  <+> thingDescD tDescT
  <> text ", but"
  <+> thingDescD tDescF
  <+> text "otherwise"
thingDescD (SCD.ContainedTDesc stDesc str) =
  text "for each contained thing,"
  <+> subThingDescD stDesc
  <> text ", separated by"
  <+> strLitD str
thingDescD (SCD.ConcatTDesc tDescs) =
  aux tDescs
  where
    aux :: [SCD.ThingDesc] -> Doc
    aux []       = empty
    aux [tDesc]  = thingDescD tDesc
    aux (td:tds) = thingDescD td
                   <+> text "+"
                   <+> aux tds

subThingDescD :: SCD.SubThingDesc -> Doc
subThingDescD (SCD.DefaultSubTDesc) = text "its description"
subThingDescD (SCD.CustomSubTDesc tDesc) = thingDescD tDesc


actionDescD :: SCD.ActionDesc -> Doc
actionDescD (SCD.LiteralADesc str) =
  strLitD str
actionDescD (SCD.IfADesc cond aDescT aDescF) =
  text "if"
  <+> conditionD cond
  <+> text "then"
  <+> actionDescD aDescT
  <> text ", but"
  <+> actionDescD aDescF
  <+> text "otherwise"
actionDescD (SCD.PlayerADesc stDesc) =
  text "description by"
  <+> subThingDescD stDesc
  <+> text "of the player"
actionDescD (SCD.LocationADesc stDesc) =
  text "description by"
  <+> subThingDescD stDesc
  <+> text "of the current location"
actionDescD (SCD.ConcatADesc aDescs) =
  aux aDescs
  where
    aux :: [SCD.ActionDesc] -> Doc
    aux []       = empty
    aux [aDesc]  = actionDescD aDesc
    aux (ad:ads) = actionDescD ad
                   <+> text "+"
                   <+> aux ads

classDeclD :: SCD.ClassDecl -> Doc
classDeclD SCD.ClassDecl{..} =
  text "A"
  <+> idD classId
  <+> if null classStats
        then empty
        else text "has"
             <+> statsD classStats
             <+> text "and"
  <+> text "is described by"
  <+> thingDescD classDesc
  <> text "."

thingThingDeclD :: SCD.ThingDecl -> Doc
thingThingDeclD thing = thingDeclD "Thing" thing

locationThingDeclD :: SCD.ThingDecl -> Doc
locationThingDeclD thing = thingDeclD "Location" thing

thingDeclD :: String -> SCD.ThingDecl -> Doc
thingDeclD kind SCD.ThingDecl{..} =
  text kind
  <+> idD thingId
  <+> text "is a"
  <+> idD thingClass
  <+> text "named" <+> nameD name
  <+> if null stats
        then empty
        else text "with" <+> statsD stats
  <+> if null contents
        then empty
        else text "that contains" <+> thingsD contents
  <> text "."


thingsD :: Set.Set SD.Id -> Doc
thingsD thingSet = thingsListD $ Set.toList thingSet
  where 
    thingsListD [] = empty
    thingsListD [id] = text "thing" <+> idD id
    thingsListD ids = text "things" <+> listD idD ids


actionDeclD :: SCD.ActionDecl -> Doc
actionDeclD SCD.ActionDecl{..} = 
  text "Action"
  <+> nameD actionName
  <+> text "is available when"
  <+> conditionD condition
  <> text ", modifies the player by"
  <+> modD modifyPlayer
  <> text ", modifies the current location by"
  <+> modD modifyCurrentLocation
  <+> (case newLocation of
         Just id -> text "before setting the current location to"
                    <+> idD id
         Nothing -> empty)
  <> text ", and is described by"
  <+> actionDescD actionDesc
  <> text "."
actionDeclD SCD.GameEndDecl{..} = 
  text "Action"
  <+> nameD actionName
  <+> text "is available when"
  <+> conditionD condition
  <> text ", ends the game, and is described by"
  <+> actionDescD actionDesc
  <> text "."


declD :: SCD.Decl -> Doc
declD (SCD.ClassDecl' cDecl)  = classDeclD cDecl
declD (SCD.ThingDecl' tDecl)  = thingThingDeclD tDecl
declD (SCD.ActionDecl' aDecl) = actionDeclD aDecl

declsD :: SCD.Decls -> Doc
declsD SCD.Decls{..} = classDeclsD $+$ thingDeclsD $+$ actionDeclsD
  where
    classDeclsD = fsep $ fmap classDeclD classDecls
    thingDeclsD = fsep $ fmap thingThingDeclD thingDecls
    actionDeclsD = fsep $ fmap actionDeclD actionDecls

playerDeclD :: SCD.PlayerDecl -> Doc
playerDeclD SCD.PlayerDecl{..} =
  text "The player"
  <+> (if null playerStats
         then empty
         else text "has"
              <+> statsD playerStats
              <+> text "and")
  <+> (if null playerThings
         then empty
         else text "has"
              <+> thingsD playerThings
              <+> text "and")
  <+> (if null playerDesc
         then empty
         else text "and is described by"
              <+> thingDescD playerDesc)
  <+> text "starts in"
  <+> idD playerStart
  <> text "."

worldDescDeclD :: SCD.WorldDescDecl -> Doc
worldDescDeclD (SCD.WorldDescDecl actDesc) =
  text "The game state is described by"
  <+> actionDescD actDesc
  <> text "."

stageD :: SCD.Stage -> Doc
stageD SCD.Stage{..} = (fsep $ fmap declD firstDeclList)
                       $+$ (if playerDeclFirst
                             then playerDeclD playerDecl
                                  $+$ (fsep $ fmap declD middleDeclList)
                                  $+$ worldDescDeclD worldDescDecl
                             else worldDescDeclD worldDescDecl
                                  $+$ (fsep $ fmap declD middleDeclList)
                                  $+$ playerDeclD playerDecl)
                       $+$ (fsep $ fmap declD lastDeclList)
  where firstDeclList = take 1 decls
        middleDeclList = take 1 (drop 1 decls)
        lastDeclList = drop 2 decls
        playerDeclFirst = (length decls) `mod` 2 == 0




