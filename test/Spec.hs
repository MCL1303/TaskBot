import Test.Tasty       (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))

import Bot (handleUpdates)

import Testing (runTesting, tMessage, tUpdate)

main :: IO ()
main = defaultMain $ testGroup ""
    [ testAdd4NotesAndShowOld
    -- , testAdd4NotesAndShowNew
    ]

testAdd4NotesAndShowOld :: TestTree
testAdd4NotesAndShowOld = testCase "add 4 notes and /showOld" $ do
    (_, traces) <- runTesting $
        handleUpdates
            [ tUpdate tMessage
            , tUpdate tMessage
            , tUpdate tMessage
            , tUpdate tMessage
            ]
    traces @=? []
