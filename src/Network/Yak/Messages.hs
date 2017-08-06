{-# LANGUAGE DataKinds #-}
module Network.Yak.Messages
(
    Channel (..),
    Message (..),

    Pass,
    Nick,
    User,
    Oper,
    Join,
    Part,
    Quit,
    SQuit,
    PrivMsg,
    Notice,
    Topic
)
where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.Text (Text)
import Data.Text.Encoding
import Data.List.NonEmpty (NonEmpty)
import Network.Yak.Types
import Data.Word (Word)

import qualified Data.Text as T

newtype Channel = Channel { getChannel :: Text }
    deriving (Eq, Show, Ord, Read)

instance Parameter Channel where
    render = render . getChannel
    seize  = do
        mark <- satisfy (inClass "&#+!")
        name <- many1 $ satisfy (notInClass " \7,\n")
        pure . Channel . T.pack $ mark : name

newtype Message = Message { getMessage :: Text }
    deriving (Eq, Show, Ord, Read)

instance Parameter Message where
    render = render . T.cons ':' . getMessage
    seize  = Message . decodeUtf8 <$> (char ':' *> takeTill (inClass "\n"))

type Pass = Msg "PASS" '[Text]
type Nick = Msg "NICK" '[Text]
type User = Msg "USER" '[Text, Word, Unused "*", Message]
type Oper = Msg "OPER" '[Text, Text]

type Join = Msg "JOIN" '[NonEmpty Channel]
type Part = Msg "PART" '[NonEmpty Channel]
type Quit = Msg "QUIT" '[Message]
type SQuit = Msg "SQUIT" '[Text, Message]

type PrivMsg = Msg "PRIVMSG" '[NonEmpty Channel, Message]
type Notice = Msg "NOTICE" '[NonEmpty Channel, Message]

type Topic = Msg "TOPIC" '[Channel, Message]
