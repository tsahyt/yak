{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Network.Yak.Messages
import Network.Yak

main :: IO ()
main = hspec $ do
    connReg
    chanOps
    srvQueries
    msgSending
    usrQueries
    miscMsgs

connReg :: Spec
connReg = describe "Connection Registration" $ do
    describe "Pass" $ do
        it "takes one argument" $ do
            fetch "PASS foo bar\n" `shouldBe` (Nothing :: Maybe Pass)

        it "succeeds parsing 'PASS hunter2'" $ do
            fetch "PASS hunter2\n" `shouldNotBe` (Nothing :: Maybe Pass)

    describe "Join" $ do
        it "takes at least one argument" $ do
            fetch "JOIN\n" `shouldBe` (Nothing :: Maybe Pass)

        it "take Channel arugments" $ do
            fetch "JOIN #haskell\n" `shouldNotBe` (Nothing :: Maybe Pass)

        it "supports multiple arguments in a list" $ do
            fetch "JOIN #haskell,#math\n" `shouldNotBe` (Nothing :: Maybe Pass)

    describe "Oper" $ do
        it "takes two arguments" $ do
            fetch "OPER\n" `shouldBe` (Nothing :: Maybe Oper)
            fetch "OPER a b c\n" `shouldBe` (Nothing :: Maybe Oper)
            fetch "OPER a b\n" `shouldNotBe` (Nothing :: Maybe Oper)

chanOps :: Spec
chanOps = return ()

srvQueries :: Spec
srvQueries = return ()

msgSending :: Spec
msgSending = return ()

usrQueries :: Spec
usrQueries = return ()

miscMsgs :: Spec
miscMsgs = return ()
