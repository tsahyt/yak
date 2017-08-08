-- | Messages for IRC Clients, as defined by RFC 2812,
-- <https://tools.ietf.org/html/rfc2812>. This does not include the responses.
-- See "Network.Yak.Response" for RFC-compliant responses.
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
type Nick    = Msg "NICK" '[Nickname]
type User    = Msg "USER" '[Username, Word, Unused "*", Message]
type Server  = Msg "SERVER" '[Target, Word, Message]
type Oper    = Msg "OPER" '[Username, Text]

-- Channel Operations -----------

type Join    = Msg "JOIN" '[NonEmpty Channel]
type Part    = Msg "PART" '[NonEmpty Channel, Maybe Message]
type Quit    = Msg "QUIT" '[Message]
type SQuit   = Msg "SQUIT" '[Target, Message]
-- TODO: Modes
type Names   = Msg "NAMES" '[[Channel], Maybe Target]
type List    = Msg "LIST" '[[Channel], Maybe Target]
type Invite  = Msg "INVITE" '[Nickname, Channel]
type Kick    = Msg "KICK" '[NonEmpty Channel, NonEmpty Nickname, Maybe Message]
type Topic   = Msg "TOPIC" '[Channel, Maybe Message]

-- Server Queries ---------------

type Version = Msg "VERSION" '[Maybe Target]
type Motd    = Msg "MOTD" '[Maybe Target]

-- Sending Messages -------------

type PrivMsg = Msg "PRIVMSG" '[NonEmpty Channel, Message]
type Notice  = Msg "NOTICE" '[NonEmpty Channel, Message]

-- User-based Queries -----------

type Who     = Msg "WHO" '[Maybe Mask, Flag "o"]
type WhoIs   = Msg "WHOIS" '[Maybe Target, NonEmpty Mask]
type WhoWas  = Msg "WHOWAS" '[NonEmpty Nickname, Maybe (Word, Maybe Target)]

-- Misc -------------------------

type Ping    = Msg "PING" '[Target, Maybe Target]
type Pong    = Msg "PONG" '[Text, Maybe Text]
type Error   = Msg "ERROR" '[Message]
