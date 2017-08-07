{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
module Main where

import Test.Hspec
import Network.Yak.Messages
import Network.Yak.Types
import Network.Yak
import Data.ByteString.Char8 (ByteString)
import GHC.TypeLits

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
        it "has format 'PASS <password>'" $ do
            (build "hunter2" :: Pass) `shouldRoundtrip` "PASS hunter2\n"

    describe "Nick" $ do
        it "has format 'NICK <nickname>'" $ do
            (build "tsahyt" :: Nick) `shouldRoundtrip` "NICK tsahyt\n"

    describe "User" $ do
        it "takes 4 arguments" $ do
            fetch "USER a 0 *\n" `shouldBe` (Nothing :: Maybe User)
            fetch "USER a 0\n" `shouldBe` (Nothing :: Maybe User)
            fetch "USER a\n" `shouldBe` (Nothing :: Maybe User)

        it "succeeds parsing 'USER guest 0 * :Real Name" $ do
            fetch "USER guest 0 * :Real Name\n" `shouldBe`
                (Just (build "guest" 0 Unused (Message "Real Name")) ::
                    Maybe User)

    describe "Oper" $ do
        it "takes two arguments" $ do
            fetch "OPER\n" `shouldBe` (Nothing :: Maybe Oper)

    describe "Server" $ do
        it "takes three arguments" $ do
            fetch "SERVER irc.tsahyt.com 1" `shouldBe` (Nothing :: Maybe Server)
            fetch "SERVER irc.tsahyt.com" `shouldBe` (Nothing :: Maybe Server)

        it "succeeds parsing 'SERVER irc.tsahyt.com 1 :IRC Server'" $ do
            fetch "SERVER irc.tsahyt.com 1 :IRC Server" `shouldBe`
                (Just (build "irc.tsahyt.com" 1 (Message "IRC Server")) ::
                    Maybe Server)

chanOps :: Spec
chanOps = describe "Channel Operations" $ do
    describe "Join" $ do
        it "take Channel arugments" $ do
            fetch "JOIN #haskell\n" `shouldNotBe` (Nothing :: Maybe Join)

        it "supports multiple arguments in a list" $ do
            fetch "JOIN #haskell,#math\n" `shouldNotBe` (Nothing :: Maybe Join)

        it "has format 'JOIN <channel>{,<channel>}'" $ do
            (build [Channel "#haskell", Channel "#math"] :: Join)
                `shouldRoundtrip` "JOIN #haskell,#math\n"

    describe "Part" $ do
        it "take Channel arugments" $ do
            fetch "PART #haskell\n" `shouldNotBe` (Nothing :: Maybe Part)

        it "supports multiple arguments in a list" $ do
            fetch "PART #haskell,#math\n" `shouldNotBe` (Nothing :: Maybe Part)

        it "has format 'PART <channel>{,<channel>} [<message>]'" $ do
            (build [Channel "#haskell", Channel "#math"] 
                   (Just (Message "bye")) :: Part)
                `shouldRoundtrip` "PART #haskell,#math :bye\n"

            (build [Channel "#haskell"] Nothing :: Part)
                `shouldRoundtrip` "PART #haskell \n"

    describe "Quit" $ do
        it "has format 'QUIT <message>'" $ do
            (build (Message "bye") :: Quit) `shouldRoundtrip` "QUIT :bye\n"

    describe "SQuit" $ do
        it "has format 'SQUIT <server> <message>'" $ do
            (build "irc.tsahyt.com" (Message "bye") :: SQuit) 
                `shouldRoundtrip` "SQUIT irc.tsahyt.com :bye\n"

    describe "Mode" $ do
        it "has format 'MODE ...'" $ do
            expectationFailure "TODO: Implement MODE"

    describe "Names" $ do
        it "has format 'NAMES [<channel>{,<channel>} [<target>]]'" $ do
            (build [Channel "#haskell"] (Just "target") :: Names) 
                `shouldRoundtrip` "NAMES #haskell target\n"

            (build [Channel "#haskell"] Nothing :: Names) 
                `shouldRoundtrip` "NAMES #haskell \n"

            (build [] Nothing :: Names) 
                `shouldRoundtrip` "NAMES  \n"

    describe "List" $ do
        it "has format 'LIST [<channel>{,<channel>} [<target>]]'" $ do
            (build [Channel "#haskell"] (Just "target") :: List)
                `shouldRoundtrip` "LIST #haskell target\n"

    describe "Invite" $ do
        it "has format 'INVITE <nickname> <channel>'" $ do
            (build "tsahyt" (Channel "#haskell") :: Invite)
                `shouldRoundtrip` "INVITE tsahyt #haskell\n"

    describe "Kick" $ do
        it "has format 'KICK <channel>{,<channel>} <nick>{,<nick>}\
          \ [<reason>];" $ do
            (build [Channel "#haskell", Channel "#math"]
                   ["tsahyt", "botnet"] (Just (Message "bye")) :: Kick)
                `shouldRoundtrip` "KICK #haskell,#math tsahyt,botnet :bye\n"

    describe "Topic" $ do
        it "has format 'TOPIC <channel> [<topic>]'" $ do
            (build (Channel "#haskell") (Just $ Message "hello world") :: Topic)
                `shouldRoundtrip` "TOPIC #haskell :hello world\n"

            (build (Channel "#haskell") Nothing :: Topic)
                `shouldRoundtrip` "TOPIC #haskell \n"

srvQueries :: Spec
srvQueries = describe "Server Queries" $ do
    describe "Version" $ do
        it "has format 'VERSION [<target>]'" $ do
            (build (Just "irc.tsahyt.com") :: Version) `shouldRoundtrip`
                "VERSION irc.tsahyt.com\n"

    describe "Motd" $ do
        it "has format 'MOTD [<target>]'" $ do
            (build (Just "irc.tsahyt.com") :: Motd) `shouldRoundtrip`
                "MOTD irc.tsahyt.com\n"

msgSending :: Spec
msgSending = describe "Sending Messages" $ do
    describe "PrivMsg" $ do
        it "takes multiple destinations in a list" $ do
            fetch "PRIVMSG #tsahyt,#math :hello world\n" `shouldNotBe` 
                (Nothing :: Maybe PrivMsg)

        it "has format 'PRIVMSG <receiver>{,receiver} :<text>'" $ do
            (build [Channel "#tsahyt", Channel "#math"] 
                   (Message "hello world") :: PrivMsg) `shouldRoundtrip` 
                    "PRIVMSG #tsahyt,#math :hello world\n"

    describe "Notice" $ do
        it "takes multiple destinations in a list" $ do
            fetch "NOTICE #tsahyt,#math :hello world\n" `shouldNotBe` 
                (Nothing :: Maybe Notice)

        it "has format 'NOTICE <receiver>{,receiver} :<text>'" $ do
            (build [Channel "#tsahyt", Channel "#math"] 
                   (Message "hello world") :: Notice) `shouldRoundtrip` 
                    "NOTICE #tsahyt,#math :hello world\n"

usrQueries :: Spec
usrQueries = describe "User-based Queries" $ do
    describe "Who" $ do
        it "has format 'WHO [<mask>] [o]'" $ do
            (build (Just "*.fi") Nothing :: Who) `shouldRoundtrip`
                "WHO *.fi \n"
            (build (Just "*.fi") (Just Unused) :: Who) `shouldRoundtrip`
                "WHO *.fi o\n"

    describe "WhoIs" $ do
        it "has format 'WHOIS [<target>] <mask>{,<mask>}'" $ do
            (build (Just "eff.org") ["trillian"] :: WhoIs) `shouldRoundtrip`
                "WHOIS eff.org trillian\n"

    describe "WhoWas" $ do
        it "has format 'WHOWAS <nick>{,<nick>} [<count> [<target>]]" $ do
            expectationFailure "TODO: Test Case"

miscMsgs :: Spec
miscMsgs = describe "Miscellaneous" $ do
    describe "Ping" $ do
        it "has format 'PING <server> [<server>]'" $ do
            (build "irc.tsahyt.com" Nothing :: Ping) `shouldRoundtrip`
                "PING irc.tsahyt.com \n"
            (build "irc.tsahyt.com" (Just "foo") :: Ping) `shouldRoundtrip`
                "PING irc.tsahyt.com foo\n"

    describe "Pong" $ do
        it "has format 'PONG <server> [<server>]'" $ do
            (build "irc.tsahyt.com" Nothing :: Pong) `shouldRoundtrip`
                "PONG irc.tsahyt.com \n"
            (build "irc.tsahyt.com" (Just "foo") :: Pong) `shouldRoundtrip`
                "PONG irc.tsahyt.com foo\n"

    describe "Error" $ do
        it "has format 'ERROR <message>'" $ do
            (build (Message "fubar") :: Error) `shouldRoundtrip`
                "ERROR :fubar\n"

shouldRoundtrip 
    :: (Parameter (PList p), Show (PList p), Eq (PList p), KnownSymbol c) 
    => Msg c p -> ByteString 
    -> Expectation
shouldRoundtrip x y = do
    emit x `shouldBe` y
    fetch y `shouldBe` Just x
