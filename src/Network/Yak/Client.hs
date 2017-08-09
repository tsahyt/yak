-- | Messages for IRC Clients, implementing the current "living standard", to be
-- found at <https://modern.ircdocs.horse/>. This module does not include the
-- responses.  See "Network.Yak.Response" for standard-compliant responses.
{-# LANGUAGE DataKinds #-}
module Network.Yak.Client
(
    -- * Connection Messages
    Cap,
    Authenticate,
    Pass,
    Nick,
    User,
    Oper,
    Quit,

    -- * Channel Operations
    Join,
    Join0,
    Part,
    Topic,
    Names,
    List,

    -- * Server Queries and Commands
    Motd,
    Version,
    Admin,
    Connect,
    Time,
    Stats,
    Info,
    Mode,

    -- * Sending Messages
    Privmsg,
    Notice,

    -- * Optional Messages
    Userhost,

    -- * Miscellaneous Messages
    Kill
)
where

import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)
import Network.Yak.Types
import Data.Word (Word)

type TODO = ()

-- Connection Messages
-- | > CAP <subcommand> [:<capabilities>]
type Cap = Msg "CAP" '[Text, Message]

-- | > AUTHENTICATE
type Authenticate = Msg "AUTHENTICATE" '[]

-- | > PASS <password>
type Pass = Msg "PASS" '[Text]

-- | > NICK <nickname>
type Nick = Msg "NICK" '[Nickname]

-- | > USER <username> 0 * <realname>
type User = Msg "USER" '[Username, Word, Unused "*", Message]

-- | > OPER <name> <password>
type Oper = Msg "OPER" '[Nickname, Text]

-- | > QUIT [<reason>]
type Quit = Msg "QUIT" '[Message]


-- Channel Operations
-- | > JOIN <channel>{,<channel>} [<key>{,<key>}]
type Join = Msg "JOIN" '[NonEmpty Channel, NonEmpty Text]
type Join0 = Msg "JOIN" '[Unused "0"]

-- | > PART <channel>{,<channel>} [<reason>]
type Part = Msg "PART" '[NonEmpty Channel, Message]

-- | > TOPIC <channel> [<topic>]
type Topic = Msg "TOPIC" '[Channel, Message]

-- | > NAMES [<channel>{,<channel>}]
type Names = Msg "NAMES" '[NonEmpty Channel]

-- | > LIST [<channel>{,<channel>}] [<elistcond>{,<elistcond>}]
type List = Msg "LIST" '[NonEmpty Channel, Maybe Text]


-- Server Queries and Commands
-- | > MOTD [<target>]
type Motd = Msg "MOTD" '[Target]

-- | > VERSION [<target>]
type Version = Msg "VERSION" '[Target]

-- | > ADMIN [<target>]
type Admin = Msg "ADMIN" '[Target]

-- | > CONNECT <target server> [<port> [<remote server>]]
type Connect = Msg "CONNECT" '[Target, Maybe (Int, Maybe Target)]

-- | > TIME [<server>]
type Time = Msg "TIME" '[Target]

-- | > STATS <query> [<server>]
type Stats = Msg "STATS" '[TODO, Maybe Target]

-- | > INFO [<target>]
type Info = Msg "INFO" '[Target]

-- | > MODE <target> [<modestring> [<mode arguments>...]]
type Mode = Msg "MODE" '[Target, TODO]

-- Sending Messages
-- | > PRIVMSG <target>{,<target>} <text to be sent>
type Privmsg = Msg "PRIVMSG" '[NonEmpty (Either Channel Nickname), Message]

-- | > NOTICE <target>{,<target>} <text to be sent>
type Notice = Msg "NOTICE" '[NonEmpty (Either Channel Nickname), Message]

-- Optional Messages
-- | > USERHOST <nickname>{ <nickname>}
type Userhost = Msg "USERHOST" '[NonEmpty Nickname]

-- Miscellaneous Messages
-- | > KILL <nickname> <comment>
type Kill = Msg "KILL" '[Nickname, Message]
