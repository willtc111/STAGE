module StageParserTests where

import StageParser
import Test.HUnit

stageParserTests :: Test
stageParserTests =
  TestList [
    symbolsTests,
    pfChoicesTests,
    pfListTests,
    pAnTests,
    pConditionTests,
    pPredTests,
    pModTests,
    pThingDescTests,
    pActionDescTests,
    pClassDeclTests,
    pThingDeclTests,
    pActionDeclTests,
    pDeclTests,
    pPlayerDeclTests,
    pStageTests ]

main :: IO ()
main = do _ <- runTestTT stageParserTests
          return ()


pAnTests :: Test
pAnTests = TestList []

symbolsTests :: Test
symbolsTests = TestList[]

pfChoicesTests :: Test
pfChoicesTests = TestList []

pfListTests :: Test
pfListTests = TestList[]

pConditionTests :: Test
pConditionTests = TestList[]

pPredTests :: Test
pPredTests = TestList[]

pModTests :: Test
pModTests = TestList[]

pThingDescTests :: Test
pThingDescTests = TestList[]

pActionDescTests :: Test
pActionDescTests = TestList[]

pClassDeclTests :: Test
pClassDeclTests = TestList[]

pThingDeclTests :: Test
pThingDeclTests = TestList[]

pActionDeclTests :: Test
pActionDeclTests = TestList[]

pDeclTests :: Test
pDeclTests = TestList[]

pPlayerDeclTests :: Test
pPlayerDeclTests = TestList[]

pStageTests :: Test
pStageTests = TestList[]

