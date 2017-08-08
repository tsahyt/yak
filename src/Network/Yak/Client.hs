{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Yak.Client
(
    -- * Connection Registration
    Pass,
    Nick,
    User,
    Server,
    Oper,

    -- * Channel Operations
    Join,
    Part,
    Quit,
    SQuit,
    Names,
    List,
    Invite,
    Kick,
    Topic,

    -- * Server Queries
    Version,
    Motd,

    -- * Sending Messages
    PrivMsg,
    Notice,

    -- * User-based Queries
    Who,
    WhoIs,
    WhoWas,

    -- * Misc
    Ping,
    Pong,
    Error
)
where

import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)
import Network.Yak.Types
import Data.Word (Word)

-- Connection Registration ------

type Pass    = Msg "PASS" '[Text]
type Nick    = Msg "NICK" '[Text]
type User    = Msg "USER" '[Text, Word, Unused "*", Message]
type Server  = Msg "SERVER" '[Text, Word, Message]
type Oper    = Msg "OPER" '[Text, Text]

-- Channel Operations -----------

type Join    = Msg "JOIN" '[NonEmpty Channel]
type Part    = Msg "PART" '[NonEmpty Channel, Maybe Message]
type Quit    = Msg "QUIT" '[Message]
type SQuit   = Msg "SQUIT" '[Text, Message]
-- TODO: Modes
type Names   = Msg "NAMES" '[[Channel], Maybe Text]
type List    = Msg "LIST" '[[Channel], Maybe Text]
type Invite  = Msg "INVITE" '[Text, Channel]
type Kick    = Msg "KICK" '[NonEmpty Channel, NonEmpty Text, Maybe Message]
type Topic   = Msg "TOPIC" '[Channel, Maybe Message]

-- Server Queries ---------------

type Version = Msg "VERSION" '[Maybe Text]
type Motd    = Msg "MOTD" '[Maybe Text]

-- Sending Messages -------------

type PrivMsg = Msg "PRIVMSG" '[NonEmpty Channel, Message]
type Notice  = Msg "NOTICE" '[NonEmpty Channel, Message]

-- User-based Queries -----------

-- TODO: Proper Mask type
type Who     = Msg "WHO" '[Maybe Text, Maybe (Unused "o")]
type WhoIs   = Msg "WHOIS" '[Maybe Text, NonEmpty Text]
type WhoWas  = Msg "WHOWAS" '[Text, Maybe (Word, Maybe Text)]

-- Misc -------------------------

type Ping    = Msg "PING" '[Text, Maybe Text]
type Pong    = Msg "PONG" '[Text, Maybe Text]
type Error   = Msg "ERROR" '[Message]
