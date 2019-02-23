{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Test.Hspec
import Network.Yak.Client
import Network.Yak.Types
import Network.Yak
import Data.ByteString.Char8 (ByteString)
import GHC.TypeLits

main :: IO ()
main = hspec $ do
    clientMsgs

clientMsgs :: Spec
clientMsgs = describe "Client Messages" $ do
    describe "Pass" $ do
        it "takes one password argument" $ do
            "PASS secretpasswordhere\n" `shouldFetch`
                Just (build "secretpasswordhere" :: Pass)
    
    describe "Nick" $ do
        it "takes one nickname argument" $ do
            "NICK Wiz\n" `shouldFetch`
                Just (build "Wiz" :: Nick)

            ":WiZ NICK Kilroy\n" `shouldFetch`
                Just (buildPrefix (PrefixUser (Host "WiZ" Nothing Nothing))
                        "Kilroy" :: Nick)

            ":dan-!d@localhost NICK Mamoped\n" `shouldFetch`
                Just (buildPrefix 
                        (PrefixUser (Host "dan-" (Just "d") (Just "localhost")))
                        "Mamoped" :: Nick)

    describe "User" $ do
        it "has format 'USER <username> 0 * <realname>'" $ do
            "USER guest 0 * :Ronnie Reagan\n" `shouldFetch`
                Just (build "guest" 0 Unused "Ronnie Reagan" :: User)

    describe "Oper" $ do
        it "takes two arguments" $ do
            "OPER foo bar\n" `shouldFetch`
                Just (build "foo" "bar" :: Oper)

    describe "Quit" $ do
        it "takes a message" $ do
            "QUIT :Gone to have lunch\n" `shouldFetch`
                Just (build "Gone to have lunch" :: Quit)

            ":dan-!d@localhost QUIT :Quit: Bye for now!\n" `shouldFetch`
                Just (buildPrefix 
                        (PrefixUser (Host "dan-" (Just "d") (Just "localhost")))
                        "Quit: Bye for now!" :: Quit)

    describe "Join" $ do
        it "has format 'JOIN <channel>{,<channel>} [<key>{,<key>}]'" $ do
            "JOIN #foobar\n" `shouldFetch`
                Just (build ["#foobar"] [] :: Join)

            "JOIN &foo fubar\n" `shouldFetch`
                Just (build ["&foo"] ["fubar"] :: Join)

            "JOIN #foo,&bar fubar\n" `shouldFetch`
                Just (build ["#foo","&bar"] ["fubar"] :: Join)

            "JOIN #foo,#bar fubar,foobar\n" `shouldFetch`
                Just (build ["#foo","#bar"] ["fubar","foobar"] :: Join)

            "JOIN #foo,#bar\n" `shouldFetch`
                Just (build ["#foo","#bar"] [] :: Join)

            ":WiZ JOIN #Twilight_zone\n" `shouldFetch`
                Just (buildPrefix (PrefixUser (Host "WiZ" Nothing Nothing))
                      ["#Twilight_zone"] [] :: Join)

            ":dan-!d@localhost JOIN #test\n" `shouldFetch`
                Just (buildPrefix 
                        (PrefixUser (Host "dan-" (Just "d") (Just "localhost")))
                        ["#test"] [] :: Join)

    describe "Part" $ do
        it "has format 'PART <channel>{,<channel>} [<reason>]'" $ do
            "PART #twilight_zone\n" `shouldFetch`
                Just (build ["#twilight_zone"] Nothing :: Part)

    describe "Topic" $ do
        it "sets topic" $ do
            "TOPIC #test :New topic\n" `shouldFetch`
                Just (build "#test" (Just "New topic") :: Topic)

        it "deletes topic" $ do
            "TOPIC #test :\n" `shouldFetch`
                Just (build "#test" (Just "") :: Topic)

        it "queries topic" $ do
            "TOPIC #test\n" `shouldFetch`
                Just (build "#test" Nothing :: Topic)

    describe "Names" $ do
        it "takes channel arguments" $ do
            "NAMES #twilight_zone,#42\n" `shouldFetch`
                Just (build ["#twilight_zone", "#42"] :: Names)

        it "takes 0 arguments" $ do
            "NAMES\n" `shouldFetch`
                Just (build [] :: Names)

    describe "List" $ do
        it "can take no argument" $ do
            "LIST\n" `shouldFetch`
                Just (build [] Nothing :: List)

        it "can take channel arguments" $ do
            "LIST #twilight_zone,#42\n" `shouldFetch`
                Just (build ["#twilight_zone", "#42"] Nothing :: List)

        it "can take query arguments" $ do
            "LIST >3\n" `shouldFetch`
                Just (build [] (Just ">3") :: List)

    describe "Version" $ do
        it "has format 'VERSION [<target>]'" $ do
            ":Wiz VERSION *.se\n" `shouldFetch`
                Just (buildPrefix (PrefixUser (Host "Wiz" Nothing Nothing))
                      (Just "*.se") :: Version)

    describe "Connect" $ do
        it "has format 'CONNECT <target> [<port> [<target>]]'" $ do
            "CONNECT  eff.org 12765 csd.bu.edu\n" `shouldFetch`
                Just (build "eff.org" (Just (12765, Just "csd.bu.edu")) 
                       :: Connect)

    describe "Stats" $ do
        it "has format 'STATS <query> [<server>]'" $ do
            "STATS m\n" `shouldFetch`
                Just (build 'm' Nothing :: Stats)

    describe "Mode" $ do
        it "has format 'MODE <target> [<modestring> [<mode arguments>..]]'" $ do
            "MODE dan +i\n" `shouldFetch`
                Just (build (Right "dan") (Just "+i\n") :: Mode)

            "MODE #foobar +mb *@127.0.0.1\n" `shouldFetch`
                Just (build (Left "#foobar") (Just "+mb *@127.0.0.1\n")
                         :: Mode)

            "MODE #foobar -bl+i *@192.168.0.1\n" `shouldFetch`
                Just (build (Left "#foobar") (Just ("-bl+i *@192.168.0.1\n"))
                         :: Mode)

            ":irc.example.com MODE #foobar +o bunny\n" `shouldFetch`
                Just (buildPrefix (PrefixServer "irc.example.com")
                          (Left "#foobar") (Just ("+o bunny\n")) :: Mode)

    describe "Userhost" $ do
        it "takes space separated nicknames" $ do
            "USERHOST Wiz Michael Marty p\n" `shouldFetch`
                Just (build "Wiz" (Just "Michael") (Just "Marty") (Just "p")
                            Nothing :: Userhost)

shouldFetch 
    :: forall c p.
       (HasCallStack, Show (Msg c p), Eq (PList p), KnownSymbol c
       ,Parameter (PList p)) 
    => ByteString -> Maybe (Msg c p) -> Expectation
shouldFetch x y = fetch x `shouldBe` y
