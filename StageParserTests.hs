module StageParserTests where

import StageParser
import qualified StageData as SD
import qualified StageCompilerData as SCD

import qualified Data.Map.Strict as Map

import Text.PrettyPrint
import Test.QuickCheck


idD :: SD.Id -> Doc
idD id = text id

nameD :: String -> Doc
nameD name = text $ "\"" ++ name ++ "\""

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

predD :: SCD.Pred -> Doc
predD p = case p of
            (SCD.NotPred p')-> aux False p'
            _           -> aux True p
  where
    aux :: Bool -> SCD.Pred -> Doc
    aux _ (SCD.TruePred)            = text "is unconditional"
    aux n (SCD.IdPred id)           = text "is"
                                      <+> if n then empty else text "not"
                                      <+> text "thing"
                                      <+> idD id
    aux n (SCD.ContainsPred pred)   = text "does"
                                      <+> if n then empty else text "not"
                                      <+> text "contain something that"
                                      <+> predD pred
    aux n (SCD.ClassPred id)        = text "is"
                                      <+> if n then empty else text "not"
                                      <+> text "a"
                                      <+> idD id
    aux n (SCD.StatPred id cmp expr)   = text "has"
                                      <+> idD id
                                      <+> cmpD cmp
                                      <+> exprD expr
    aux n (SCD.OrPred pred1 pred2)  = text "either"
                                      <+> predD pred1
                                      <+> text "or"
                                      <+> predD pred2
    aux n (SCD.AndPred pred1 pred2) = text "both"
                                      <+> predD pred1
                                      <+> text "and"
                                      <+> predD pred2


cmpD :: SCD.Cmp -> Doc
cmpD SCD.EqCmp = text "="
cmpD SCD.NeCmp = text "/="
cmpD SCD.LtCmp = text "<"
cmpD SCD.LeCmp = text "<="
cmpD SCD.GtCmp = text ">"
cmpd SCD.GeCmp = text ">="


modD :: SCD.Mod -> Doc
modD (SCD.DoNothingMod)         = text "doing nothing"
modD (SCD.SetMod id expr)       = undefined -- TODO once exprD is added
modD (SCD.GiveMod id)           = text "giving it" <+> idD id
modD (SCD.TakeMod id)           = text "taking" <+> idD id <+> text "from it"
modD (SCD.IfMod pred modT modF) = undefined -- TODO once predD is added
modD (SCD.ContainsMod mod)      = text "modifying by" <+> modD mod <+> text "everything it contains"
modD (SCD.AndMod mod1 mod2)     = text "first" <+> modD mod1 <+> text "and then" <+> modD mod2

exprD :: SCD.Expr -> Doc
exprD = undefined

thingThingDeclD :: SCD.ThingDecl -> Doc
thingThingDeclD thing = thingDeclD "Thing" thing

locationThingDeclD :: SCD.ThingDecl -> Doc
locationThingDeclD thing = thingDeclD "Location" thing

thingDeclD :: String -> SCD.ThingDecl -> Doc
thingDeclD kind thing = text kind
                        <+> idD (SCD.thingId thing)
                        <+> text "is a"
                        <+> idD (SCD.thingClass thing)
                        <+> text "named" <+> nameD (SCD.name thing)
                        <+> if null (SCD.stats thing)
                              then empty
                              else text "with" <+> statsD (SCD.stats thing)
                        <+> if null (SCD.contents thing)
                              then empty
                              else text "that contains" <+> aux (SCD.contents thing)
  where
    aux [] = empty
    aux [id] = text "thing" <+> idD id
    aux ids = text "things" <+> listD idD ids


